import Balances._
import play.api.libs.json._
import Data._
import Main.Slope
import Slopes.{FallingSlope, RaisingSlope, Slope}

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}
import scala.+:
import scala.annotation.tailrec
import scala.collection.GenSeq
import scala.collection.immutable.{Seq, SeqOps}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.Try

object Main extends App {

  def timeToStr(epochMillis: Long): String =
    Instant.ofEpochMilli(epochMillis).atZone(ZoneId.systemDefault()).toLocalDateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))


  def prices = Json.parse(Source.fromResource("cryptoData.json").mkString).as[Data].prices.map { case Seq(k, v) => (k, v) }.toMap

  def pricesWithDate = prices.map { case (k, v) => (timeToStr(k.toLong), v) }


  object Slope {
    def apply(values: Double*): Slope = values match {
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
  }.map(Slope(_: _*))

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

  def isProfitable(lastValue: Double, possibleValue: Double)(implicit minProfitPercent: Double) = {
    possibleValue > lastValue + lastValue * minProfitPercent
  }

  /*  def exchangeIfProfitable(lastState: CurrencyState, currentState: CurrencyState, rate: Double) = {
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
    }*/


  /*  def doSimulation(data: Seq[Slope], inputUsdf: Double) = data.foldLeft[Seq[CurrencyState]](Seq(UsdState(inputUsdf))) {
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
    }*/

  def doSimulationRec(btcRates: Seq[Double], input: CurrencyState)(implicit minProfitPercent: Double): Seq[CurrencyState] = {

    @tailrec
    def doSimulationAccu(btcRates: Seq[Double],
                         slopesHistory: Seq[Slope],
                         stateHistory: Seq[CurrencyState]): Seq[CurrencyState] = {
      btcRates match {
        case Seq() => stateHistory
        case _ => {
          val newSlopesHistory = slopesHistory match {
            case previousHistory :+ lastSlope => previousHistory ++ lastSlope.extendWithinSeq(btcRates.head)
          }
          doSimulationAccu(btcRates.tail, newSlopesHistory, stateHistory :+ strategicExchangeOption(newSlopesHistory, stateHistory).getOrElse(stateHistory.last))
        }
      }
    }

    val oppositeCurrencyState = input match {
      case _: UsdState => BtcState(0)
      case _: BtcState => UsdState(0)
    }
    val (firstTwo, futureRates) = btcRates.splitAt(2)

    doSimulationAccu(futureRates, Seq(Slope(firstTwo: _*)), Seq(oppositeCurrencyState, input, input))
  }

  def strategicExchangeOption(slopesHistory: Seq[Slope],
                              stateHistory: Seq[CurrencyState])(implicit minProfitPercent: Double): Option[CurrencyState] = {
    val rate = slopesHistory.last.end
    val previousStateHistory :+ lastState = stateHistory

    val (possibleBtc, possibleUsd) = lastState match {
      case btc: BtcState => (btc, btc.exchangeTo(UsdState)(rate))
      case usd: UsdState => (usd.exchangeTo(BtcState)(1 / rate), usd)
    }

    def FallingToRaising = slopesChangedFromTo[FallingSlope, RaisingSlope](_)

    def RaisingToFalling = FallingToRaising.andThen(!_)

    def lastAmount[T <: CurrencyState : ClassTag] = lastCurrencyOfType[T](previousStateHistory).get.amount

    (slopesHistory, lastState) match {
      case (history, state) if FallingToRaising(history) && usdAvaliable(state) && isProfitable(lastAmount[BtcState], possibleBtc.amount) => {
        Some(possibleBtc)
      }
      case (history, state) if RaisingToFalling(history) && btcAvaliable(state) && isProfitable(lastAmount[UsdState], possibleUsd.amount) => {
        Some(possibleUsd)
      }
      case _ => None
    }
    /*    if (FallingToRaising && usdAvaliable(lastState) && isProfitable(lastAmount[BtcState], possibleBtc.amount))
          Some(possibleBtc)
        else if (RaisingToFalling && btcAvaliable(lastState) && isProfitable(lastAmount[UsdState], possibleUsd.amount))
          Some(possibleUsd)
        else
          None*/
  }

  def usdAvaliable(state: CurrencyState): Boolean = state match {
    case UsdState(x) if x > 0 => true
    case _ => false
  }

  def btcAvaliable(state: CurrencyState): Boolean = state match {
    case BtcState(x) if x > 0 => true
    case _ => false
  }

  def slopesChangedFromTo[From <: Slope : ClassTag, To <: Slope : ClassTag](slopesHistory: Seq[Slope]) = slopesHistory match {
    case Seq() => false
    case Seq(_) => false
    case _ :+ (_: From) :+ (_: To) => true
    case _ => false
  }

  def lastOfType[U, T <: U : ClassTag](history: Seq[U]) = history.findLast {
    case _: T => true
    case _ => false
  }

  def lastCurrencyOfType[T <: CurrencyState : ClassTag](history: Seq[CurrencyState]) = lastOfType[CurrencyState, T](history)

  val x = 1
  //val realisticCaseAccountHistory1 = doSimulation(slopes, inputUSD)
  val se = Seq(0, 1, 2, 3, 4).map(1d + _.toDouble / 100)
  val raising = Slope(se: _*)
  val falling = Slope(se.reverse: _*)
  val slopes1 = Seq() :+ raising :+ falling :+ raising :+ falling
  //val test1 = doSimulation(slopes1, 100d)
  val percentSlope = slopes.map(_.percentage)
  //val last100Realistic = realisticCaseAccountHistory1.takeRight(100)

  val realisticCaseAccountHistory2 = doSimulationRec(prices.values.toSeq, UsdState(inputUSD))(0)
  val last100Realistic2 = realisticCaseAccountHistory2.takeRight(100)


  def minProfitsToCheck = (1 to 100).map(_.toDouble / 1000).par

  def calculate(profit: Double) = {
    def result = doSimulationRec(prices.values.toSeq, UsdState(inputUSD))(profit)

    lastCurrencyOfType[UsdState](result)
  }

  def hours(days: Int) = days * 24

  val ratioOfSlopeEnds = 1.03

  val (Seq(slopesHead), slopesTail) = slopes.splitAt(1)
  val pairsOfSlopes = slopesTail.foldLeft(slopesHead) {
    case (last@RaisingSlope(rMin, _, rMax), next@FallingSlope(fMax, _, fMin))
      if rMax / rMin >= ratioOfSlopeEnds && fMax / fMin >= ratioOfSlopeEnds => {

    }
    case _ =>
  }



  /*val pairsOfSlopes = slopes.par.groupBy(
    slope=>BigDecimal(slope.end/slope.start).setScale(2,BigDecimal.RoundingMode.HALF_UP)
  ).map(
    x=>(x._1,x._2.size)
  ).toList.filter(_._1>1).sortBy(_._1)/*.scanRight((BigDecimal(0),0)){
    case ((percentage,count),(_,countZ))=>(percentage,count+countZ)}*/*/

  println("")
  for {
    periods <- hours(10) to hours(30) by hours(1)

  } yield periods

  val result = minProfitsToCheck.map(calculate)



  //println(prices.values)
  //prices.values.toSeq.
  println("")

}
