package one.xingyi.cddutilities.json


object JsonMaps {

  def toMap(j: JsonValue): Any = j match {
    case JsonString(s) => s
    case JsonInt(i) => i
    case JsonDouble(d) => d
    case JsonBoolean(b) => b
    case j: JsonObject => j.nameAndValues.map { case (k, v) => (k, toMap(v)) }.toMap
    case JsonList(list) => list.map(toMap)
  }

}
case class JsonMaps[J](jsonValue: JsonValue)(implicit jWriter: JsonWriter[J]) {
  lazy val map = JsonMaps.toMap(jsonValue)
  lazy val json = jWriter(jsonValue)
}

object JsonAsMaps extends JsonAsMaps
trait JsonAsMaps {
  implicit val jsonWriter: JsonWriter[Any] = new JsonWriter[Any] {
    override def toJ(jsonValue: JsonValue): Any = JsonMaps.toMap(jsonValue)
    override def toStringForJ: Any => String = _.toString
  }
}
