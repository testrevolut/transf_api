package services

import javax.inject._
import javax.inject.Inject

import scala.concurrent.{Await, ExecutionContext, Future}
import services.AccountType.AccountType
import services.Currency.CurrencyType
import scala.collection.mutable.ListBuffer

/**
  * Created by dmitri on 24/11/2016.
  */

//  manipulating whatever we keep in store
@Singleton
class DataStorage @Inject() (xe: ExchangeData) (dt: DataItself) {
  val jprocessor = new JsonProcessing()

  // just querying
  def listAccounts() = dt.listAccounts()
  def listAccounts(id: Int) = dt.listAccounts(List(id))
  def listClients() = dt.listClients()

  //actual manipulations
  def addClient(name: String, surname: String): String = {
    if (name == "" && surname == "")
      return "improper format"
    if  (dt.getClient(name, surname) != null)
      return "already client"
    else {
      val accId = dt.generateNewMax()
      val person = new Person(name, surname,ListBuffer(accId))
      val defaultAccount =  new Account(accId, 0.01, Currency.RUB, 0.01, AccountType.debit, ListBuffer())
      dt.addAccount(accId, defaultAccount)
      dt.addClient(person)
      return "done"
    }
  }
  def addAccount(name: String, surname: String, accountType: AccountType, balance: Double, currency: CurrencyType, overdraft: Double) : String = {
    val client = dt.getClient(name,surname)
    if (client != null){
      val id = dt.generateNewMax()
      val account = new Account(id,balance,currency,overdraft,accountType, ListBuffer())
      dt.addAccount(id, account)
      dt.updateClientAccounts(client,id)
      "added"
    } else {
      "not client"
    }
  }
  def deposit(id: Int, amount: Double): String = {
    val account = dt.getAccount(id)
    if (account == null)
      "unknown account"
    else
      if (dt.updateBalance(id,amount))
        "success"
      else
        "something is wrong"
  }
  def withdraw(id: Int, amount: Double): String = {
    val account = dt.getAccount(id)
    if (account == null)
      "unknown account"
    else {
      if (account.balance - amount > 0 - account.overdraftLimit){
        if (dt.updateBalance(account.id,-1.0 * amount)){
          "success"
        } else {
          "something is wrong"
        }
      } else
        "insufficient funds"
    }
  }

  def transfer(sendingAcc: Int, recName: String, recSurname: String, currency: CurrencyType, amount: Double): String = {
    // check whether sender and receiver are in the system
    val receiver = dt.getClient(recName,recSurname)
    if (receiver != null)
      "unknown addressee"
    val idx = dt.listPersonsAccounts(recName,recSurname)
    val sender = dt.getAccount(sendingAcc)
    // get rates
    val xrates = xe.getRates()
    if (xrates.isEmpty)
      "rates data not available"
    if (idx.nonEmpty && sender != null) {
      // check if there is enough money in sender's account
      val ok = sender.isOkToSend(amount,currency, xe.exchangeRates)
      if (ok) {
        // check if addressee can receive given currency
        var notreceivable = true
        var receivingId = - 1
        for (i <- idx) {
          if (dt.getAccount(i).currency == currency) {
            notreceivable = false
            receivingId = dt.getAccount(i).id
          }
        }
        if (notreceivable)
          return "not receivable"
        val receivingAccount = dt.getAccount(receivingId)
        val first = sender.currency
        // finally process the transfer
        val actualAmt = sender.convertAmount(amount,currency,first,xrates)

        val witdrawal = withdraw(sender.id,actualAmt)
        if ( witdrawal != "success"){
          return witdrawal
        } else {
          val depAmount = amount * (1 - receivingAccount.checkReceieveCommission())
          var depresult = "too little"
          if (depAmount > 0.01)
            depresult = deposit(receivingId,depAmount )
          else {
            val bounceback = deposit(sender.id, actualAmt * (1.0 + sender.checkSendCommission()))
            depresult
          }
          if (depresult != "success"){
            // bounce back
            val bounceback = deposit(sender.id, actualAmt * (1.0 + sender.checkSendCommission()))
            return "not receivable"
          }
        }
      } else {
        return "sending prohibited"
      }
      return "success"
    } else {
      if (sender == null)
        return "unknown sender"
      if (idx.isEmpty)
        return "not receivable"
    }
    "something is wrong"
  }
}




