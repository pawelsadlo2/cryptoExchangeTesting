import Balances.{BtcState, UsdState}
import Main.{Slope, lastOfType, raising, strategicExchangeOption}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class StrategicExchangeTests extends AnyFlatSpec with should.Matchers {

  import StrategicExchangeTests._

  val testedEntity = this.getClass.getName
  implicit val minProvit:Double = 0

  testedEntity should "exchange usd to btc on start of raising slope" in {
    strategicExchangeOption(raisingBegin, statesLastUsd) shouldBe a[Some[BtcState]]
  }

  testedEntity should "not exchange usd on start of falling slope" in {
    strategicExchangeOption(fallingBegin, statesLastUsd) shouldBe None
  }

  testedEntity should "exchange btc to usd after on start of falling slope" in {
    strategicExchangeOption(fallingBegin, statesLastBtc) shouldBe a[Some[UsdState]]
  }

  testedEntity should "not exchange currency we dont obtain" in {
    strategicExchangeOption(fallingBegin, statesLastUsd) shouldBe None
    strategicExchangeOption(raisingBegin, statesLastBtc) shouldBe None
  }

  testedEntity should "not do unprofitable exchange" in {

    strategicExchangeOption(raisingBegin, BtcState(150) +: statesLastUsd.tail) shouldBe None
    strategicExchangeOption(fallingBegin, UsdState(150) +: statesLastBtc.tail) shouldBe None
  }

}

object StrategicExchangeTests {
  private val slope1 = Slope(1.2, 1.1, 1)
  private val slope2 = Slope(1, 1.1, 1.2)
  private val fallingToRaising = Seq() :+ slope1 :+ slope2
  private val raisingToFalling = fallingToRaising.reverse
  private val fallingBegin = slope2.extendWithinSeq(1.1)
  private val raisingBegin = slope1.extendWithinSeq(1.1)

  private val statesLastUsd = Seq() :+ BtcState(0) :+ UsdState(100) :+ UsdState(100)
  private val statesLastBtc = Seq() :+ UsdState(0) :+ BtcState(100) :+ BtcState(100)
}