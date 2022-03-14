object Balances {
  sealed trait CurrencyState{

  }

  final case class BtcState(amount:Double) extends CurrencyState


  final case class UsdState(amount:Double) extends CurrencyState

}
