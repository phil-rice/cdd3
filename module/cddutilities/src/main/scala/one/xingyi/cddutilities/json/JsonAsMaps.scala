package one.xingyi.cddutilities.json


object JsonMaps {
  def apply(j: JsonValue): JsonMaps = JsonMaps(toMap(j))

  private def toMap(j: JsonValue): Any = j match {
    case JsonString(s) => s
    case JsonInt(i) => i
    case JsonDouble(d) => d
    case j: JsonObject => j.nameAndValues.map { case (k, v) => (k, toMap(v)) }.toMap
    case JsonList(list) => list.map(toMap)
  }

}
case class JsonMaps(map: Any)

object JsonAsMaps extends JsonAsMaps
trait JsonAsMaps {
  implicit val jsonWriter: JsonWriter[Any] = new JsonWriter[Any] {
    override def toJ(jsonValue: JsonValue): Any = jsonValue match {
      case JsonString(s) => s
      case JsonInt(i) => i
      case JsonDouble(d) => d
      case j: JsonObject => Map(j.nameAndValues.map { case (k, v) => (k, toJ(v)) }: _*)
      case JsonList(list) => List(list.map(toJ))
    }
    override def toStringForJ: Any => String = _.toString
  }
}
