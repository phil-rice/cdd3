package one.xingyi.json4s

import one.xingyi.cddutilities.json._
import org.json4s.JsonAST.{JArray, JObject}
import org.json4s.native.JsonMethods
import org.json4s.{DefaultFormats, JValue}

import scala.language.implicitConversions

case class FromJson4sException(msg: String, cause: Throwable) extends Exception(msg, cause)

object Json4s extends Json4sParser with Json4sWriter
object Json4sParser extends Json4sParser
trait Json4sParser {
  implicit object JsonParserForJson4s extends JsonParser[JValue] {
    protected implicit val formats = DefaultFormats
    override def extractInt(j: JValue): Int = j.extract[Int]
    override def extractString(j: JValue): String = j.extract[String]
    override def extractOptString(j: JValue): Option[String] = j.extractOpt[String]
    override def asList(j: JValue): List[JValue] = j.extract[List[JValue]]
    override def \(j: JValue, s: String): JValue = j \ s
    override def apply(s: String): JValue = JsonMethods.parse(s)
  }
}



object Json4sWriter extends Json4sWriter
trait Json4sWriter {
  implicit object JsonWriterForJson4s extends JsonWriter[JValue] {
    protected implicit val formats = DefaultFormats
    import org.json4s.JsonDSL._
    override def toJ(jsonValue: JsonValue): JValue = jsonValue match {
      case JsonString(s) => s
      case JsonInt(i) => i
      case JsonDouble(d) => d
      case j: JsonObject => JObject(j.nameAndValues.map { case (k, v) => (k, toJ(v)) }: _*)
      case JsonList(list) => JArray(list.map(toJ))
    }
    override def toStringForJ = JsonMethods.render _ andThen JsonMethods.pretty
  }
}


