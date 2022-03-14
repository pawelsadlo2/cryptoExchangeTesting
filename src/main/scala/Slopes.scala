object Slopes {
  sealed trait Slope {
    val start: Double
    val end: Double
    val mid:Seq[Double]
    def percentage: Double

    def isRaising: Boolean

    def isFalling: Boolean = !isRaising

    def extendWithinSeq(elem: Double): Seq[Slope]
  }

  final case class RaisingSlope(start: Double, mid: Seq[Double], end: Double) extends Slope {

    def isRaising: Boolean = true

    def percentage: Double = (end / start - 1) * 100

    def extendWithinSeq(elem: Double): Seq[Slope] =
      if (elem >= end)
        Seq(RaisingSlope(start, mid :+ end, elem))
      else
        Seq(this, FallingSlope(end, Seq(), elem))

    override def toString(): String = s">>>$start>${mid.mkString(">")}>$end"
  }

  final case class FallingSlope(start: Double, mid: Seq[Double], end: Double) extends Slope {

    def isRaising: Boolean = false

    def percentage: Double = -(start / end - 1) * 100

    def extendWithinSeq(elem: Double): Seq[Slope] =
      if (elem < end)
        Seq(FallingSlope(start, mid :+ end, elem))
      else
        Seq(this, RaisingSlope(end, Seq(), elem))

    override def toString(): String = s"<<<$start<${mid.mkString("<")}<$end"
  }
}
