package services
import play.api.libs.json.{JsObject, _}
import services.AccountType.AccountType
import services.Currency.CurrencyType

import scala.collection.mutable.ListBuffer
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._


/**
  * Created by dmitri on 24/11/2016.
  */
case class ExchRate(first: CurrencyType, second: CurrencyType, rate: Double, ts: Long)
// case classes for json requests we need to validate
case class TransactionRequestMessage(id: Int, amount: Double)
case class TransferRequestMessage(id: Int, name: String, surname: String, currency: String, amount: Double)
case class NewAccRequest(name: String, surname: String, acctype: String, balance: Double, currency: String, overdraft: Double)
case class NewCustomerRequest(name: String, surname: String)

class JsonProcessing {
  // dealing with exchange rates (downloaded from external API)
  def processRates(rates: String): List[ExchRate] = {
    val result = ListBuffer[ExchRate]()
    val jstr = Json.parse(rates)
    val ts = (jstr \ "timestamp").get.as[Long]
    //println(System.currentTimeMillis()/1000)
    for(curr <- Currency.values) {
      val quote = (jstr \ "quotes" \ ("USD" + curr.toString)).get.as[Double]
      if (quote.isInstanceOf[Double]){
        val rate = ExchRate(Currency.USD,curr,"%.4f".format(quote).toDouble,ts)
        val revers = ExchRate(curr,Currency.USD,"%.4f".format(1.0/quote).toDouble,ts)
        if (curr != Currency.USD) {
          result.+=(rate)
          result.+=(revers)
        }
      }
    }
    result.toList
  }
  def makeJsonRateList(rates: List[ExchRate]): JsValue = {
    var quotes = Json.arr()
    for (quote <- rates) {
      quotes = quotes.+:(
        Json.obj(
          "first" -> quote.first,
          "second" -> quote.second,
          "rate" -> "%.4f".format(quote.rate).toDouble
        )
      )
    }
    val cls = Json.obj("rates" -> quotes)
    cls
  }

  // helper to be used in future
  def initializeHistory(): JsValue = {
    val ts = (System.currentTimeMillis() / 1000).toInt
    val record = Json.obj("created" -> ts)
    return record
  }

  // generating return values
  def displayClients(req: List[Person]): JsValue = {
    var persons = Json.arr()
    for (person <- req) {
      persons = persons.+:(
        Json.obj(
          "name" -> person.name,
          "surname" -> person.surname,
          "accounts" -> person.accounts

        )
      )
    }

    val cls = Json.obj("clients" -> persons)
    cls
  }
  def displayAccounts(req: List[Account]): JsValue = {
    var listOfAccs = Json.arr()
    for (acc <- req) {
      listOfAccs = listOfAccs.+:(
        Json.obj(
          "type" -> acc.accountType,
          "currency" -> acc.currency,
          "id" -> acc.id,
          "balance" -> "%.2f".format(acc.balance).toDouble
        )
      )
    }
    return Json.obj("accounts" -> listOfAccs)
  }



  // validation of case classes above
  def validateNewCustomerRequest(message: JsValue): JsResult[NewCustomerRequest] = {
    val nameReads = (
      (JsPath \ "name").read[String] and (JsPath \ "surname").read[String]
      ) (NewCustomerRequest.apply _)
    message.validate[NewCustomerRequest](nameReads)
  }
  def validateNewAccountRequest(message: JsValue): (JsResult[NewAccRequest], CurrencyType, AccountType) = {
    val nameReads = (
      (JsPath \ "name").read[String] and
        (JsPath \ "surname").read[String] and
        (JsPath \ "accounttype").read[String] and
        (JsPath \ "balance").read[Double] and
        (JsPath \ "currency").read[String] and
        (JsPath \ "overdraft").read[Double]
      ) (NewAccRequest.apply _)
    val nameResult = message.validate[NewAccRequest](nameReads)

    val acctType = nameResult.get.acctype match {
      case "debit" => AccountType.debit
      case "credit" => AccountType.credit
      case "premium" => AccountType.premium
      case _ => null
    }
    val currType = nameResult.get.currency match {
      case "USD" => Currency.USD
      case "RUB" => Currency.RUB
      case "EUR" => Currency.EUR
      case "GBP" => Currency.GBP
    }
    return (nameResult, currType, acctType)
  }
  def validateTransactionRequest(message: JsValue): JsResult[TransactionRequestMessage] = {
    val tReads = ((JsPath \ "id").read[Int] and (JsPath \ "amount").read[Double]) (TransactionRequestMessage.apply _)
    message.validate[TransactionRequestMessage](tReads)
  }
  def validateTransferRequest(message: JsValue): (JsResult[TransferRequestMessage], CurrencyType) = {
    val tReads = (
      (JsPath \ "id").read[Int] and
        (JsPath \ "name").read[String] and
        (JsPath \ "surname").read[String] and
        (JsPath \ "currency").read[String] and
        (JsPath \ "amount").read[Double]
      ) (TransferRequestMessage.apply _)
    val tResult = message.validate[TransferRequestMessage](tReads)
    var curr: CurrencyType = null
    if (tResult.isSuccess)
      curr = tResult.get.currency match {
        case "USD" => Currency.USD
        case "RUB" => Currency.RUB
        case "EUR" => Currency.EUR
        case "GBP" => Currency.GBP
      }
    return (tResult, curr)
  }
  def validateInt(message: JsValue): JsResult[Int] = {
    val intReads = ((JsPath \ "id").read[Int])
    message.validate[Int](intReads )
  }
}
