package one.xingyi.cddmustache
import one.xingyi.cddutilities.ToHtmlWithJson
import one.xingyi.cddutilities.json.JsonWriter

case class MustacheToHtml[J: JsonWriter, T](templateName: String, title: String)(implicit mustacheBuilder: MustacheBuilder) extends ToHtmlWithJson[T] {
  val mf = mustacheBuilder(templateName, title)
  override def apply(t: T) = { json => mf.apply(t, json) }
}
