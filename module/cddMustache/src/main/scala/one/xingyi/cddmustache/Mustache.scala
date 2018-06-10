package one.xingyi.cddmustache

import java.io.StringWriter

import com.github.mustachejava.{DefaultMustacheFactory, Mustache => JMustache}
import com.twitter.mustache.ScalaObjectHandler
import one.xingyi.cddutilities.json.{JsonMaps, JsonWriter, ToJson}


object Mustache {
  val mf = new DefaultMustacheFactory()
  mf.setObjectHandler(new ScalaObjectHandler)
  def apply(name: String) = new Mustache(mf.compile(name))
  def withTemplate(template: String) = new MustacheBuilder(template)
}

class MustacheBuilder(template: String) {
  def apply(name: String, title: String) = new MustacheWithTemplate(Mustache(template), Mustache(name), title)
}

case class BodyJsonAndTitle(body: String, json: String, title: String)

class MustacheWithTemplate(template: Mustache, main: Mustache, title: String) {
  def apply[J](item: JsonMaps[J]) = {
    val str = main(item)
    template(BodyJsonAndTitle(str, item.json, title))
  }
}

class Mustache(mustache: JMustache) {
  def apply(item: Any) = {
    val writer = new StringWriter()
    mustache.execute(writer, item)
    writer.flush()
    writer.toString
  }
}
case class MustacheToHtmlAndJson[J: JsonWriter, T](templateName: String, title: String)(implicit mustacheBuilder: MustacheBuilder) {
  val mf = mustacheBuilder(templateName, title)
  //  override def apply(json: String, t: T) = {mf.apply(t, json)}
}
