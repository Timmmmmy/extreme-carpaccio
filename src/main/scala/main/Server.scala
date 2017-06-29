package main

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.ContentTypes.`application/json`
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.{HttpApp, Route}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.generic.auto._

case class Order(prices: Seq[Float], quantities: Seq[Int], country: String, reduction: String)

case class Result(total: Float)

case class Feedback(`type`: String, content: String)

object Server extends HttpApp with App {

  type ProcessOrder = Order => Option[Float]


  val remises = Map("STANDARD" -> Map(1000 -> 3, 5000 -> 5, 7000 -> 7, 10000 -> 10, 50000 -> 15))
  val taxMap = Map("DE" -> 20, "UK" -> 21, "FR" -> 20, "IT" -> 25, "ES" -> 19, "PL" -> 21, "RO" -> 20, "NL" -> 20, "BE" -> 24, "EL" -> 20, "CZ" -> 19, "PT" -> 23, "HU" -> 27, "SE" -> 23, "AT" -> 22, "BG" -> 21, "DK" -> 21, "FI" -> 17, "SK" -> 18, "IE" -> 21, "HR" -> 23, "LT" -> 23, "SI" -> 24, "LV" -> 20, "EE" -> 22, "CY" -> 21, "LU" -> 25, "MT" -> 20)

  private def computeTotal(order: Order) = {
    var total: Float = 0

    for {
      price <- order.prices
      quantity <- order.quantities
    } {
      total += price * quantity
    }

    total
  }

  private def computeTaxes(country: String, total: Float): Float = {
    val tax = taxMap.getOrElse(country, 0).toFloat

    total + total * (tax / 100)
  }

  private def computeRemises(reduction: String, total: Float): Float = {
    val remise = remises.get(reduction).getOrElse(Map.empty)
    val percentage = remise.keys.filter(x => total >= x).last

    total - total * (remise.get(percentage).map(_.toFloat).getOrElse(0f) / 100)
  }

  lazy val process: ProcessOrder = {
    order => {
      val total = computeTotal(order)
      Some(computeRemises(order.reduction, computeTaxes(order.country, total)))

    }
  }
  //  {
  // TODO To implement
  //    order => {
  //      computeTotal
  //      order.prices.zip(order.quantities).fold(0.0)(x => x._1)
  //      val total: Float =
  //        for {
  //        prices <- order.prices
  //        quantity <- order.quantities
  //      } yield {
  //        prices * quantity
  //      }
  //
  //      total
  //    }
  //  }

  override def routes: Route =
    path("order") {
      post {
        entity(as[Order]) { order =>
          complete {
            println("Request received : " + order)

            val total = process(order)

            Result(total.getOrElse(0f))
          }
        }
      }
    } ~
      path("feedback") {
        post {
          entity(as[Feedback]) { feedback =>
            complete {
              println("Feedback received : " + feedback)
              ""
            }
          }
        }
      }

  startServer(host = "0.0.0.0", port = 9000)

}