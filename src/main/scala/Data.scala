import play.api.libs.json.{Format, Json}

case class Data(prices: Seq[Seq[Double]],
                market_caps: Seq[Seq[Double]],
                total_volumes: Seq[Seq[Double]]
               )
object Data{
  implicit val dataFormat: Format[Data] = Json.format[Data]
}