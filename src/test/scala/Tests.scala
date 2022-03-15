import Balances.{BtcState, UsdState}
import Main.{Slope, doSimulationRec, lastCurrencyOfType, lastOfType, slopesChangedFromTo}
import Slopes.{FallingSlope, RaisingSlope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Tests extends AnyFlatSpec with should.Matchers {

  import Tests._

  "UsdState" should "be possible to convert to BtcState" in {
    assert(UsdState(1).exchangeTo(BtcState)(1) == BtcState(0.999))
  }

  "slopesChangedFromTo" should "work for empty Seq" in {
    slopesChangedFromTo[RaisingSlope, FallingSlope](Seq()) shouldBe false
  }

  "slopesChangedFromTo" should "work for one slope" in {
    slopesChangedFromTo[RaisingSlope, FallingSlope](Seq() :+ slope1) shouldBe false
  }

  "slopesChangedFromTo" should "detect slopes change" in {

    slopesChangedFromTo[FallingSlope, RaisingSlope](fallingToRaising) shouldBe true
    slopesChangedFromTo[RaisingSlope, FallingSlope](raisingToFalling) shouldBe true

    slopesChangedFromTo[FallingSlope, RaisingSlope](fallingToRaising ++ fallingToRaising) shouldBe true
    slopesChangedFromTo[FallingSlope, RaisingSlope](raisingToFalling ++ fallingToRaising) shouldBe true
  }

  "lastStateOfType" should "detect slopes change" in {

    lastCurrencyOfType[UsdState](Seq()) shouldBe None
    lastCurrencyOfType[BtcState](Seq()) shouldBe None

    lastCurrencyOfType[BtcState](statesLastBtc) shouldBe a[Some[BtcState]]
    lastCurrencyOfType[BtcState](statesLastBtc) shouldBe Some(BtcState(100))

    lastCurrencyOfType[UsdState](statesNoUsd) shouldBe None

    lastCurrencyOfType[UsdState](statesLastUsd) shouldBe Some(UsdState(100))

    lastCurrencyOfType[UsdState](statesLastUsd:+BtcState(1)) shouldBe Some(UsdState(100))
    lastCurrencyOfType[UsdState](statesLastUsd:+UsdState(1)) shouldBe Some(UsdState(1))
  }

  "doSimulationRec" should "compute" in{
    val rates:Seq[Double] = Seq(4,3,2,1,2,3,4,3,2,1,2,3,4).map(1+_.toDouble/100)
    val result = doSimulationRec(rates,UsdState(100))
    result shouldBe Seq()
  }
}
object Tests {
  implicit val minPercentProfit:Double = 0
  val slope1 = Slope(1.2, 1.1, 1)
  val slope2 = Slope(1, 1.1, 1.2)
  val fallingToRaising = Seq() :+ slope1 :+ slope2
  val raisingToFalling = fallingToRaising.reverse
  val fallingBegin = slope2.extendWithinSeq(1.1)
  val raisingBegin = slope1.extendWithinSeq(1.1)

  val statesLastUsd = Seq() :+ BtcState(0) :+ UsdState(100) :+ UsdState(100)
  val statesLastBtc = Seq() :+ UsdState(0) :+ BtcState(100) :+ BtcState(100)
  val statesNoUsd = Seq()  :+ BtcState(100) :+ BtcState(100)
}

