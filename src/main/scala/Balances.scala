object Balances {

  trait Exchanger[-From, To] {
    def exchange(source: From): Double => To
  }

  sealed trait CurrencyState {
    val amount: Double

    def to[To](exchanger: Exchanger[this.type, To]): Double => To
  }

  final case class BtcState(amount: Double) extends CurrencyState {
    def to[To](exchanger: Exchanger[BtcState.this.type, To]): Double => To = exchanger.exchange(this)
  }

  object BtcToUsd extends Exchanger[BtcState, UsdState] {
    def exchange(source: BtcState): Double => UsdState = rate => {
      def fee = source.amount * 0.001

      UsdState((source.amount - fee) * rate)
    }
  }

  object UsdToBtc extends Exchanger[UsdState, BtcState] {
    def exchange(source: UsdState): Double => BtcState = rate => {
      def fee = source.amount * 0.001

      BtcState((source.amount - fee) * rate)
    }
  }

  final case class UsdState(amount: Double) extends CurrencyState {
    def to[To](exchanger: Exchanger[UsdState.this.type, To]): Double => To = exchanger.exchange(this)
  }

}
