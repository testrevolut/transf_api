package services

import javax.inject.Singleton

import play.api.libs.json.JsValue

/**
  * Created by dmitri on 28/11/2016.
  */
// pseudo-datastore (two mutable maps actually): only basic operations on stored data allowed
@Singleton
class DataItself {
  val jprocessor = new JsonProcessing()

  private var clients = scala.collection.mutable.Map[(String,String),Person]()
  private var accounts =  scala.collection.mutable.Map[Int,Account]()
  private var accountMaxId: Int = 1000000

  def generateNewMax(): Int = {
    val ret = accountMaxId + 1
    accountMaxId = accountMaxId + 1
    ret
  }
  def addAccount(id: Int, account: Account) = {
    accounts.+=(id -> account)
  }
  def addClient(client: Person) = {
    clients.+=((client.name, client.surname) -> client)
  }
  def updateBalance(id: Int, amount: Double): Boolean = {
    val res  = false
    if (accounts.contains(id)) {
      accounts(id).balance.+=(amount)
      true
    } else {
      false
    }
  }
  def updateClientAccounts(client: Person, id: Int) = {
    clients(client.name, client.surname).accounts+=(id)
  }
  def getClient(name: String, surname: String): Person = {
    clients.getOrElse((name,surname),null)
  }
  def getAccount(id: Int): Account = {
    accounts.getOrElse(id, null)
  }
  // display whatever is inside as json
  def listAccounts() = {
    jprocessor.displayAccounts(accounts.values.toList)
  }
  def listAccounts(ids: List[Int]) = {
    val filterSet = ids.toSet
    val accts = accounts.values.toList.filter(x => filterSet.contains(x.id))
    jprocessor.displayAccounts(accts)
  }
  def listPersonsAccounts(name: String, surname: String): List[Int] = {
    val person = clients.getOrElse((name,surname),null)
    if (person != null)
      person.accounts.toList
    else
      null
  }
  def listClients(): JsValue = {
    jprocessor.displayClients(clients.values.toList)
  }
}

