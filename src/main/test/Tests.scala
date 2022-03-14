import Balances.{BtcState, UsdState}
import Main.{Slope, slopesChangedFromTo, strategicExchangeOption}
import Slopes.{FallingSlope, RaisingSlope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Tests extends AnyFlatSpec with should.Matchers{
  "UsdState" should "be possible to convert to BtcState" in {
    assert(UsdState(1).exchangeTo(BtcState)(1)==BtcState(0.999))
  }

  "StrategicExchange" should "be possible to convert to BtcState" in {
    val slopes = Seq(Slope(1.2,1.1,1),Slope(1,1.1,1.2))
    val states = Seq() :+ BtcState(0):+ UsdState(100):+ UsdState(100)
    assert(strategicExchangeOption(slopes,states)==None)
  }

  "ss" should "be possible to convert to BtcState" in {
    val slopes = Seq(Slope(1.2,1.1,1),Slope(1,1.1,1.2))
    val res =slopesChangedFromTo[FallingSlope, RaisingSlope](slopes)
    assert(res)
  }


}
