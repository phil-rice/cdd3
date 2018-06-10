package one.xingyi.cddutilities.json

import one.xingyi.cddutilities.{Monoid, SemiGroup}

import scala.language.implicitConversions


trait ToJson[T] extends (T => String)

object ToJson {
  implicit def default[J, T](implicit jsonWriter: JsonWriter[J], toJsonLib: ToJsonLib[T]): ToJson[T] = t => jsonWriter(toJsonLib(t))
}


sealed trait JsonValue
case class JsonString(s: String) extends JsonValue
case class JsonInt(i: Int) extends JsonValue
case class JsonBoolean(b: Boolean) extends JsonValue
case class JsonDouble(d: Double) extends JsonValue
case class JsonObject(nameAndValues: (String, JsonValue)*) extends JsonValue {
  def|+|(other: (String, JsonValue)*) = JsonObject((nameAndValues ++ other): _*)
}
case class JsonList(list: List[JsonValue]) extends JsonValue

object JsonObject {

  implicit def semiGroupForJsonObject: SemiGroup[JsonObject] = { (left, right) => JsonObject((left.nameAndValues ++ right.nameAndValues): _*) }
}

trait JsonWriter[J] extends (JsonValue => String) {
  def toJ(jsonValue: JsonValue): J
  def toStringForJ: J => String
  def apply(jsonValue: JsonValue): String = toStringForJ(toJ(jsonValue))
}

trait ToJsonLib[T] extends (T => JsonValue)

object JsonWriterLangauge extends JsonWriterLangauge
trait JsonWriterLangauge {
  implicit def toJsonString(s: String): JsonValue = JsonString(s)
  implicit def toJsonInt(i: Int) = JsonInt(i)
  implicit def toJsonDouble(d: Double) = JsonDouble(d)
  implicit def toJsonBoolean(b: Boolean) = JsonBoolean(b)
}
