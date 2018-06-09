package one.xingyi.cddmustache
import one.xingyi.cddutilities.ToHtmlAndJson
import one.xingyi.cddutilities.json.JsonWriter

case class MustacheToHtmlAndJson[J: JsonWriter, T](templateName: String, title: String)(implicit mustacheBuilder: MustacheBuilder) extends ToHtmlAndJson[T] {
  val mf = mustacheBuilder(templateName, title)
  override def apply(json: String, t: T) = {mf.apply(t, json)}
}
