package one.xingyi.cddexamples

import one.xingyi.cddengine._
import one.xingyi.cddmustache.{Mustache, MustacheBuilder, MustacheWithTemplate}
import one.xingyi.cddscenario.InternetDocument
import one.xingyi.cddutilities.LeftRightTree
import one.xingyi.cddutilities.json.{JsonMaps, JsonObject, JsonValue, JsonWriter}
import org.json4s.JsonAST.JValue
class Tennis {
  val definition = InternetDocument("CodingDojo", "http://codingdojo.org/cgi-bin/wiki.pl?KataTennis")
  val wikipedia = InternetDocument("Wikipedia", "http://en.wikipedia.org/wiki/Tennis_score")
  val changeRequest = InternetDocument("CR24", "http://en.wikipedia.org/wiki/Tennis_score")

  val lookup = Map(0 -> "love", 1 -> "fifteen", 2 -> "thirty", 3 -> "forty")

  type TennisUseCase = UseCase2[Int, Int, String]
  val ucLeftWins = new TennisUseCase("left winning") {
    scenario(4, 0) produces "left won" when { (l, r) => (l - r) >= 2 && l >= 4 }
    scenario(4, 1) produces "left won"
    scenario(4, 2) produces "left won"
    scenario(5, 3) produces "left won"
    scenario(5, 3) produces "left won"
  }
  //  reference("2.1", definition).
  val ucRightWins = new TennisUseCase("Receiver winning") {
    scenario(0, 4) produces "right won" when { (l: Int, r: Int) => (r - l) >= 2 && r >= 4 }
    scenario(1, 4) produces "right won"
    scenario(2, 4) produces "right won"
    scenario(3, 5) produces "right won"
    scenario(40, 42) produces "right won"
  }
  val ucRunningScore = new TennisUseCase("Running score") {
    //  reference("2.10", definition)
    scenario(2, 3) produces "thirty, forty" because { case (l, r) if l < 4 && r < 4 => s"${lookup(l)}, ${lookup(r)}" }
    scenario(2, 1) produces "thirty, fifteen"
  }
  val ucXXAll = new TennisUseCase("When both have the same running score") {
    //                 reference("2.11", definition).
    scenario(0, 0) produces "love all" because { case (l, r) if l == r && l < 3 => s"${lookup(l)} all" }
    scenario(2, 2) produces "thirty all"

  }
  val ucDeuce = new TennisUseCase("Deuce") {
    //  reference("5", definition).
    scenario(3, 3) produces "deuce" when { (l, r) => l >= 3 && r >= 3 && l == r }
    scenario(4, 4) produces "deuce"
    scenario(6, 6) produces "deuce"
  }

  val ucAdvantage = new TennisUseCase("Advantage") {
    //  reference("3", definition).
    scenario(5, 4) produces "advantage left" when { (l, r) => l >= 3 && r >= 3 && l == r + 1 }
    scenario(6, 5) produces "advantage left" //.reference("2").
    scenario(4, 3) produces "advantage left"

    scenario(4, 5) produces "advantage right" when { (l, r) => l >= 3 && r >= 3 && r == l + 1 }
    scenario(5, 6) produces "advantage right"
  }
  val tennis = Engine(ucLeftWins or ucRightWins or ucRunningScore or ucXXAll or ucDeuce or ucAdvantage)
  def dump = {
    println(tennis(1, 1))
    println(tennis(2, 1))
    println(tennis(3, 1))
    println(tennis(3, 2))
    println(tennis(3, 3))
    println(tennis(4, 4))
    println(tennis(4, 5))
    println(tennis(5, 5))
    println(tennis(6, 5))
    println(tennis(7, 5))
  }
}

object JsonDataForTree {
  def make[J: JsonWriter, P, R](data: WithScenarioData[P, R])(jsonObject: JsonObject): JsonDataForTree[J, P, R] = JsonDataForTree[J, P, R](jsonObject, data)
}
case class JsonDataForTree[J: JsonWriter, P, R](jsonObject: JsonObject, data: WithScenarioData[P, R]) extends JsonMaps[J](jsonObject)
object Tennis extends Tennis with App {
  import one.xingyi.cddutilities.FunctionLanguage._
  import one.xingyi.json4s.Json4s._
  //  import one.xingyi.cddutilities.json.JsonAsMaps._
  //  dumpprintln(
  println("started")
  private val tree = tennis.asInstanceOf[Engine1[(Int, Int), String]].dt
  println("made tree")
  implicit val template: MustacheWithTemplate = Mustache.withTemplate("main.template.mustache") apply("decisiontree.mustache", "Tennis")
  private val simple = DecisionTreeRendering.simple[(Int, Int), String]
  val printer = simple andThen JsonMaps.apply[JValue] andThen template.apply
  def tracePrinter[P, R](data: WithScenarioData[P, R]) = DecisionTreeRendering.withScenario[P, R](data) andThen JsonDataForTree.make[JValue, P, R](data)  andThen template.apply
  println("made printer")

  //  println(printer(tree))
  import one.xingyi.cddmustache._
  //  implicit def toHtml[P, R] = MustacheToHtmlAndJson[JValue, JsonMaps]("decisiontree.mustache", "Tennis")
  println
  println
  println
  println("doing it")
  private val jsonObject = simple.tree(tree)
  println(jsonObject)
  println("done simple")
  JsonMaps.toMap(jsonObject)
  println("made mapes")
  println(printer.tree(tree))
  //  DecisionTreeTracer.trace("target/tennis{0}.html")(tennis.asInstanceOf[Engine1[(Int, Int), String]].scenarios)
  println("tracing")
  DecisionTreeRendering.trace(tracePrinter[(Int, Int), String])("target/tennis{0}.html")(tennis.asInstanceOf[Engine1[(Int, Int), String]].scenarios)
  println("done it")
  //  dump
  //  println(DecisionNodePrinter(root))
  //  println
  //  val x = LeftRightTree(root)
  //  println(x)
}

