package services
import javax.inject.Inject

import play.api.libs.json.JsValue
import services.AccountType.AccountType
import services.Currency.CurrencyType

import scala.collection.mutable.ListBuffer
/**
  * Created by dmitri on 25/11/2016.
  */

// we can only have three types of accounts
object AccountType extends Enumeration {
  type AccountType = Value
  val debit, credit, premium = Value
}
object  Currency extends  Enumeration {
  type CurrencyType = Value
  val RUB,USD,EUR,GBP = Value
}
case class Person (name: String, surname: String, accounts:  ListBuffer[Int])

// manipulation of balances etc, convertation policies
case class Account  (
                     id: Int,
                     //holder: Person,
                     var balance: Double,
                     currency: CurrencyType,
                     var overdraftLimit: Double,
                     accountType: AccountType,
                     history: ListBuffer[JsValue]
                   ) {
  // some random commissions for transfers
  def checkSendCommission (): Double = {
    this.accountType match {
      case AccountType.debit => 0.01
      case AccountType.credit => 0.03
      case AccountType.premium => 0.005
      case _ => -1.0
    }
  }
  def checkReceieveCommission (): Double = {
    this.accountType match {
      case AccountType.debit => 0.00
      case AccountType.credit => 0.03
      case AccountType.premium => 0.00
      case _ => -1.0
    }
  }

  // checks whether given amount in specified currency can be sent from the account (i.e. sufficient/insufficient funds)
  def isOkToSend(amount: Double, curr: CurrencyType, exchRate: List[ExchRate]): Boolean = {
    var amt = amount
    if (curr != this.currency){
      amt = convertAmount(amount,curr,this.currency,exchRate)
    }
    this.balance - amt * (1.0 + checkSendCommission()) > 0 - this.overdraftLimit
  }
  // converting based on account policies
  def convertAmount(amount: Double, first: CurrencyType,second: CurrencyType, exchRates: List[ExchRate]): Double = {
    var amt = amount
    // we convert through dollar
    for (r <- exchRates) {
      if(r.first == first && r.second == Currency.USD) {
        amt = amt * r.rate
      }
    }
    for (r <- exchRates) {
      if (r.first == Currency.USD && r.second == second) {
        amt = amt * r.rate
      }
    }
    return amt
  }

}


