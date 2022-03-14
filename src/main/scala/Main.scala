import Balances._
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

  val slopes = pricesWithDate.values.foldLeft[Seq[Seq[Double]]](Seq.empty) {
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
    case (currentCount, RaisingSlope(start, mid, end)) => (for {
      btcCount <- Try(exchange(currentCount, 1 / start))
      usdCount <- Try(exchange(btcCount, end))
    } yield usdCount).get
  }

  def isProfitable(lastValue: Double, possibleValue: Double) = {
    possibleValue > lastValue
  }

  def exchangeIfProfitable(lastState: CurrencyState, currentState: CurrencyState, rate: Double) = {
    currentState match {
      case UsdState(amount) => {
        val exchanged = UsdState(amount).to(UsdToBtc)(1 / rate)
        if (exchanged.amount > lastState.amount) exchanged else lastState
      }
      case BtcState(amount) => {
        val exchanged = BtcState(amount).to(BtcToUsd)(rate)
        if (exchanged.amount > lastState.amount) exchanged else lastState
      }
    }
  }


  def doSimulation(data: Seq[Slope], inputUsdf: Double) = data.foldLeft[Seq[CurrencyState]](Seq(UsdState(inputUsdf))) {
    case (history@Seq(UsdState(usdInput)), RaisingSlope(start, _, _)) =>
      history :+ UsdState(usdInput).to[BtcState](UsdToBtc)(1 / start)
    case (history@Seq(UsdState(usdInput)), FallingSlope(start, _, _)) =>
      history :+ UsdState(usdInput).to[BtcState](UsdToBtc)(1 / start)
    case (hist@(history :+ UsdState(currentUsd)), RaisingSlope(start, mid, end)) => {
      val lastAmount = history.findLast {
        case _: BtcState => true
        case _ => false
      }.get.amount
      val possibleAmount: BtcState = UsdState(currentUsd).to(UsdToBtc)(1 / (start +: mid :+ end).tail.head)

      if (isProfitable(lastAmount, possibleAmount.amount)) hist :+ possibleAmount else hist :+ UsdState(currentUsd)
    }
    case (hist@(history :+ BtcState(currentBtc)), FallingSlope(start, mid, end)) => {
      val lastAmount = history.findLast {
        case _: UsdState => true
        case _ => false
      }.get.amount
      val possibleAmount: UsdState = BtcState(currentBtc).to(BtcToUsd)((start +: mid :+ end).tail.head)

      if (isProfitable(lastAmount, possibleAmount.amount)) hist :+ possibleAmount else hist :+ BtcState(currentBtc)
    }
    case (cantExchange, _) =>
      cantExchange :+ cantExchange.last
  }

  def doSimulationRec(btcRates: Seq[Double], input: CurrencyState): Seq[CurrencyState] = {

    def doSimulationAccu(btcRates: Seq[Double], slopesHistory: Seq[Slope], stateHistory: Seq[CurrencyState]): Seq[CurrencyState] = {
      (btcRates, slopesHistory, stateHistory) match {
        case (Seq(), _, _) => stateHistory
        case (first +: second +: tail, Seq(), Seq(state)) =>
          doSimulationAccu(tail, slopesHistory :+ Slope(Seq(first, second)), stateHistory :+ state)
        case (head +: tail, previousSlopes :+ FallingSlope(start, mid, end), previousStates :+ UsdState(amount)) if head > end =>
          doSimulationAccu(
            tail,
            previousSlopes ++ FallingSlope(start, mid, end).extendWithinSeq(head),
            stateHistory :+ exchangeIfProfitable(previousStates.findLast { case BtcState(_) => true; case _=>false }.get, UsdState(amount), head)
          )
        case (head +: tail, previousSlopes :+ RaisingSlope(start, mid, end), previousStates :+ BtcState(amount)) if head < end =>
          doSimulationAccu(
            tail,
            previousSlopes ++ RaisingSlope(start, mid, end).extendWithinSeq(head),
            stateHistory :+ exchangeIfProfitable(previousStates.findLast { case UsdState(_) => true; case _=>false }.get, BtcState(amount), head)
          )
        case (head +: tail, slopesHistory :+ last, statesHist :+ lastState) =>
          doSimulationAccu(tail, slopesHistory ++ last.extendWithinSeq(head), statesHist :+ lastState :+ lastState)
      }
    }

    doSimulationAccu(btcRates, Seq.empty, Seq(input))
  }

  val x = 1
  val realisticCaseAccountHistory1 = doSimulation(slopes, inputUSD)
  val se = Seq(0, 1, 2, 3, 4).map(1d + _.toDouble / 100)
  val raising = Slope(se)
  val falling = Slope(se.reverse)
  val slopes1 = Seq() :+ raising :+ falling :+ raising :+ falling
  val test1 = doSimulation(slopes1, 100d)
  val percentSlope = slopes.map(_.percentage)
  val last100Realistic = realisticCaseAccountHistory1.takeRight(100)

  val realisticCaseAccountHistory2 = doSimulationRec(prices.values.toSeq, UsdState(inputUSD))
  val last100Realistic2 = realisticCaseAccountHistory2.takeRight(100)
  //println(prices.values)
  println("")

}
