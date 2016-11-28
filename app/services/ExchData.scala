package services

import javax.inject.{Inject, Singleton}
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

/**
  * Created by dmitri on 28/11/2016.
  */
// these are exchange rates downloaded from some external api
@Singleton
class ExchangeData @Inject() (ws: WSClient)  {
  val apikey = "583d4836c8999a183474cd750efdd010"
  var exchangeRates = List[ExchRate]()
  val jprocessor = new JsonProcessing()
  // download new rates if the data is more than one hour old
  def updateRates () = {
    if (exchangeRates.isEmpty || System.currentTimeMillis() / 1000 - exchangeRates.head.ts > 3600)
      exchangeRates = checkRates()
  }

  def getRates (): List[ExchRate] = {
    updateRates()
    return exchangeRates
  }
  // actual downloading
  def checkRates (): List[ExchRate] = {
    val xcurl = "http://apilayer.net/api/live"
    val request: WSRequest = ws.url(xcurl)
    val complexRequest: WSRequest =
      request
        .withHeaders("Accept" -> "application/json")
        .withRequestTimeout(10000.millis)
        .withQueryString(
          "access_key" -> apikey,
          "currencies" -> Currency.values.mkString(",")
        )
    val futureResponse: Future[WSResponse] = complexRequest.get()
    val res = Await.result(futureResponse, 5000.millis)
    jprocessor.processRates(res.body)
  }
}