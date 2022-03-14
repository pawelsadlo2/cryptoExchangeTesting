object Slopes {
  sealed trait Slope{
    def isRaising:Boolean
    def isFalling:Boolean = !isRaising
  }

  final case class RaisingSlope(start: Double, mid: Seq[Double], end: Double) extends Slope{
    def isRaising:Boolean = true
  }

  final case class FallingSlope(start: Double, mid: Seq[Double], end: Double) extends Slope{
    def isRaising:Boolean = false
  }
}
