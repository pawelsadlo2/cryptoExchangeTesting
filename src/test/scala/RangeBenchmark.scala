import Main.{calculate, minProfitsToCheck}
import org.scalameter.api._

object RangeBenchmark
  extends Bench.LocalTime {


  performance of "Range" in {
    measure method "map" in {
      minProfitsToCheck.map(calculate)
    }
  }
}