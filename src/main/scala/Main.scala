import Balances.{BtcState, CurrencyState, UsdState}
import play.api.libs.json._
import Data._
import Main.Slope
import Slopes.{FallingSlope, RaisingSlope, Slope}

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}
import scala.+:
import scala.io.Source
import scala.util.Try

object Main extends App {
  def timeToStr(epochMillis: Long): String =
    Instant.ofEpochMilli(epochMillis).atZone(ZoneId.systemDefault()).toLocalDateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))


  def prices = Json.parse(Source.fromResource("cryptoData.json").mkString).as[Data].prices.map { case Seq(k, v) => (k, v) }.toMap

  def pricesWithDate = prices.map { case (k, v) => (timeToStr(k.toLong), v) }




  object Slope {
    def apply(values: Seq[Double]): Slope = values match {
      case Seq() => throw new Exception
      case Seq(_) => throw new Exception
      case first +: mid :+ last if first <= last => RaisingSlope(first, mid, last)
      case first +: mid :+ last if first > last => FallingSlope(first, mid, last)
    }
  }

  def slopes = pricesWithDate.values.foldLeft[Seq[Seq[Double]]](Seq.empty) {
    case (slopesAccu, nextRate) => slopesAccu match {
      case Seq() => slopesAccu :+ Seq(nextRate)
      case pre :+ last => last match {
        case Seq(singleElement) => pre :+ Seq(singleElement, nextRate)
        case start +: mid :+ end if end >= start && nextRate >= end => pre :+ (start +: mid :+ end :+ nextRate) /*nonEmpty.updated(nonEmpty.length - 1, (start, elem))*/
        //continue ascending or flat
        case start +: mid :+ end if end < start && nextRate < end => pre :+ (start +: mid :+ end :+ nextRate) /*nonEmpty.updated(nonEmpty.length - 1, (start, elem))*/
        //continue descending
        case start +: mid :+ end if end >= start && nextRate < end => slopesAccu :+ Seq(end, nextRate) /*nonEmpty appended(end, elem)*/
        // start of ascending
        case start +: mid :+ end if end < start && nextRate > end => slopesAccu :+ Seq(end, nextRate) /*nonEmpty appended(end, elem)*/
        // start of ascending
      }
    }
  }.map(Slope(_))

  val raisingSlopes = slopes.filter(_.isRaising)
  //val bestProfits = raisingSlopes.map { case start +: _ :+ end => end - start }

  def exchange(amount: Double, rate: Double) = {
    def fee = amount * 0.001

    (amount - fee) * rate
  }

  val inputUSD = 100d //USD

  val bestCaseAccountHistory = raisingSlopes.scanLeft(inputUSD) {
    case (currentCount, RaisingSlope(start ,mid, end)) => (for {
      btcCount <- Try(exchange(currentCount, 1 / start))
      usdCount <- Try(exchange(btcCount, end))
    } yield usdCount).get
  }

  def isProfitable(lastValue: Double, possibleValue: Double) = {
    possibleValue > lastValue
  }

  val realisticCaseAccountHistory1 = slopes.foldLeft[Seq[CurrencyState]](Seq(UsdState(inputUSD))) {
    case (Seq(UsdState(usdInput)),RaisingSlope(start,_,_))=>Seq():+UsdState(usdInput) :+ BtcState(exchange(inputUSD, 1 / start))
    case (Seq(UsdState(usdInput)),FallingSlope(start,_,_))=>Seq():+UsdState(usdInput) :+ BtcState(exchange(inputUSD, 1 / start))
    case (,RaisingSlope(start,mid,end)) =>
    case (zero,FallingSlope(start,mid,end)) =>
  }


 /* val realisticCaseAccountHistory = slopes.foldLeft(Seq((inputUSD, 0d))) {
    case (Seq((inputUSD, 0d)), start +: _) =>
      Seq((inputUSD, 0d)) :+ (0d, exchange(inputUSD, 1 / start))
    case (history :+ ((previousUSD, previousBTC)) :+ ((currentUSD, currentBTC))
    , start +: xs) if currentBTC == 0d =>
      if (isProfitable(previousBTC, exchange(currentUSD, 1 / xs.head)))
        history :+ ((previousUSD, previousBTC)) :+ ((currentUSD, currentBTC)) :+ (0d, exchange(currentUSD, 1 / xs.head))
      else
        history :+ ((previousUSD, previousBTC)) :+ ((currentUSD, currentBTC)) :+ ((currentUSD, currentBTC))
    case (history :+ ((previousUSD, previousBTC)) :+ ((currentUSD, currentBTC))
    , start +: xs) if currentUSD == 0d =>
      if (isProfitable(previousUSD, exchange(currentBTC, xs.head)))
        history :+ ((previousUSD, previousBTC)) :+ ((currentUSD, currentBTC)) :+ ((exchange(currentBTC, xs.head), 0d))
      else
        history :+ ((previousUSD, previousBTC)) :+ ((currentUSD, currentBTC)) :+ ((currentUSD, currentBTC))
  }*/


  println("")

}
