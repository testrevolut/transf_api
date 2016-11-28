package controllers

import javax.inject._

import akka.actor.Status.Success
import jdk.nashorn.internal.ir.ObjectNode
import org.json4s.JsonAST.JString
import play.api.data._
import play.api.libs.ws.{WSClient, WSRequest}
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.i18n.MessagesApi
import play._

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure
import play.api.libs.json.{JsObject, _}
import play.api.mvc.BodyParsers.parse
import services.{Currency, _}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.json
import services.AccountType.AccountType
import services.Currency.CurrencyType
//import services.JsonProcessing.{NewAccRequest, NewCustomerRequest, TransactionRequestMessage, TransferRequestMessage} // Combinator syntax

/**
  * Created by dmitri on 13/11/2016.
  */
class Transfer @Inject() (ds: DataStorage)(xe: ExchangeData) extends Controller {
  val js = new JsonProcessing
  def index = Action {
    Ok("the service is up and running")
  }
  // here we can  either return json of display actual html with bells and whistles
  def getCurrencyExchanges() = Action { request =>
    val rates = xe.checkRates
    request.headers.get("format").getOrElse("nada") match {
      case "json" => Ok(js.makeJsonRateList(rates))
      case _ => Ok(views.html.rates("Pretentious display of exchange rates","blah",rates ))
    }
  }

  // display what is in the system
  def listAccounts() = Action {
    Ok(ds.listAccounts())
  }
  def getAccount() = Action { request =>
    val jr = request.body.asJson.get
    val intRes = js.validateInt(jr)
    intRes match {
      case s: JsSuccess[Int] =>
        Ok(ds.listAccounts(intRes.get))
      case e: JsError => BadRequest("the request did not validate")
    }
  }
  def listClients() = Action {
    Ok(ds.listClients())
  }

  // create new things
  def addClient() = Action { request =>
    val jr = request.body.asJson.get
    val nameResult = js.validateNewCustomerRequest(jr)
    nameResult match {
      case s: JsSuccess[NewCustomerRequest] =>
        val q = ds.addClient(nameResult.get.name, nameResult.get.surname)
        q match {
          case "done" => Created(q)
          case "already client" => Conflict(q)
          case "improper format" => UnprocessableEntity(q)
          case _ => BadRequest
        }
      case e: JsError => BadRequest("the request did not validate")
    }
  }
  def addAccount = Action { implicit request =>
    val jr = request.body.asJson.get
    val (nameResult, currType, acctType) = js.validateNewAccountRequest(jr)
    nameResult match {
      case s: JsSuccess[NewAccRequest] =>
        val q = ds.addAccount(
          nameResult.get.name,
          nameResult.get.surname,
          acctType,
          nameResult.get.balance,
          currType,
          nameResult.get.overdraft
        )
        q match {
          case "added" => Ok(q)
          case "not client" => UnprocessableEntity(q)
          case _ => BadRequest
        }
      case e: JsError => BadRequest("the request did not validate")
    }
  }

  // transform already present accounts
  def deposit = Action { request =>
    val jr = request.body.asJson.get
    val tResult = js.validateTransactionRequest(jr)
    tResult match {
      case s: JsSuccess[TransactionRequestMessage] =>
        val res = ds.deposit(tResult.get.id,tResult.get.amount)
        res match {
          case "success" => Ok("success!")
          case "something is wrong" => Conflict("could not deposit money")
          case _ => BadRequest
        }
      case e: JsError => BadRequest("the request did not validate")
    }
  }
  def transfer = Action { implicit request =>
    val jv = request.body.asJson.get
    val (trResult, curr) = js.validateTransferRequest(jv)
    trResult match {
      case s: JsSuccess[TransferRequestMessage] =>
        val res = ds.transfer(
          trResult.get.id,
          trResult.get.name,
          trResult.get.surname,
          curr,
          trResult.get.amount
        )
        res match {
          case "success" => Ok("success")
          case "not receivable" => Conflict("not receivable")
          case "sending prohibited" => Conflict("sending prohibited")
          case "unknown sender" => Conflict("unknown sender")
          case "unknown addressee" => Conflict("unknown addressee")
          case "rates data not available" => Conflict("rates data not available")
          case "too little" => Conflict("too little")
          case _ => BadRequest
        }
      case e: JsError => BadRequest("the request did not validate")
    }
  }
}
