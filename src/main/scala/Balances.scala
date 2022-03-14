import Balances.BtcState

object Balances {

  trait Exchangable {
    def exchangeTo[To <: CurrencyState](targetFactory: CurrencyFactory[To])(rate: Double): To

    val percentProvision: Double
  }


  sealed trait CurrencyState extends Exchangable {
    val amount: Double

    def exchangeTo[To <: CurrencyState](targetFactory: CurrencyFactory[To])(rate: Double): To = {

/*      this match {
        case sameCurrency: To => sameCurrency
        case _ => {*/
          val fee = this.amount * this.percentProvision

          targetFactory.create((this.amount - fee) * rate)
/*        }
      }*/

    }
  }

  final case class UsdState(amount: Double) extends CurrencyState {
    val percentProvision: Double = UsdState.percentProvision
  }

  final case class BtcState(amount: Double) extends CurrencyState {
    val percentProvision: Double = BtcState.percentProvision
  }

  trait Factory[A, B] {
    def create: A => B
  }

  sealed trait CurrencyFactory[B <: CurrencyState] extends Factory[Double, B] {
    def create: Double => B
  }

  final object BtcState extends CurrencyFactory[BtcState] {
    def create: Double => BtcState = amount => this (amount)

    final val percentProvision = 0.001
  }

  final object UsdState extends CurrencyFactory[UsdState] {
    def create: Double => UsdState = amount => this (amount)

    final val percentProvision = 0.001
  }
}
