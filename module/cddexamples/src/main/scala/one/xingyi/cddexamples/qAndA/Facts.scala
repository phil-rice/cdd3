package one.xingyi.cddexamples.qAndA
import java.util.ResourceBundle
import java.util.concurrent.Executor

import com.sun.net.httpserver.HttpExchange
import core.{HttpUtils, PathAndHandler, SimpleHttpResponse, SimpleHttpServer}
import one.xingyi.cddexamples.qAndA.Question.{blackboard, entity, newEntity}
import one.xingyi.cddmustache.{Mustache, MustacheWithTemplate}
import one.xingyi.cddutilities.functions.Lens
import one.xingyi.cddutilities.json._
import one.xingyi.cddutilities.language.AnyLanguage._
import one.xingyi.cddutilities.strings.{ShortPrint, Strings}
import one.xingyi.json4s.Json4sWriter
import org.json4s.JValue

import scala.collection.immutable
import scala.language.reflectiveCalls
case class Fact[DS, T](id: String, lens: Lens[DS, T])

case class IndustrySector(s: String)
case class Address(s: String)
case class Website(s: String)
case class TeleNo(s: String)
case class BIC(s: String)
case class ClearingCode(s: String)
case class Chaps(s: String)

case class Naics(code: String)
case class Nace(code: Int)
case class Identity(name: String,
                    industrySectory: IndustrySector,
                    registeredAddress: Address,
                    operationAddress: Address,
                    teleNo: TeleNo, fax: TeleNo,
                    website: Website,
                    bIC: BIC,
                    chaps: Chaps,
                    clearingCode: ClearingCode)

case class GBP(amnt: Int)
case class BalanceSheet(totalNetAssets: GBP, totalLiabilities: GBP, shareHoldersInterest: GBP)
case class ProfitAndLoss(nettIncome: GBP, nettExpenditure: GBP)
case class FinancialData(balanceSheet: BalanceSheet, profitAndLoss: ProfitAndLoss)

case class Activities(mainActivities: String, productAndServices: String, businessLine: String, naics: Naics, nace: Nace)

case class GateKeeperThings(bearerShares: Boolean)

case class Entity(identity: Identity, financialData: FinancialData, activities: Activities, gateKeeperThings: GateKeeperThings)

trait Manipulate[DS] {
  val identity: Lens[DS, Identity] = ???
  val status: Lens[DS, Boolean] = ???
  val balanceSheet: Lens[DS, BalanceSheet] = ???
  val profitAndLoss: Lens[DS, ProfitAndLoss] = ???
  val activites: Lens[DS, Activities] = ???
}

object Decision {
  def apply[E](items: (BlackboardItem[E, _], String)*)(implicit blackboard: Blackboard[E]) = { e: E => items.find { case (bi, _) => bi.validate(e).nonEmpty } }
}


case class NewQuestion(idToDisplayTheQuestion: String)
case class ProcessResponse[DS](idForAValue: String, lens: Lens[DS, String])
import one.xingyi.cddutilities.json.JsonLanguage._

case class ValidateProblem(s: String)

trait Blackboard[B] {
  def findLens(name: List[String]): Lens[B, String]
  def name: String
  def children: List[BlackboardItem[B, _]]
}

trait BlackboardItem[B, T] extends Blackboard[T] {
  def lens: Lens[B, T]
  def lensAndFindLens(name: List[String]): Lens[B, String] = lens andThen findLens(name)
  def validateFn: T => List[ValidateProblem]
  def validate(b: B): immutable.Seq[ValidateProblem] = validateFn(lens(b))
  def toJson(prefix: List[String], b: B)(implicit bundle: ResourceBundle): JsonValue
}
case class SimpleBlackboardItem[B, T](name: String, lens: Lens[B, T], validateFn: T => List[ValidateProblem])(implicit stringLens: Lens[T, String]) extends BlackboardItem[B, T] {
  override def children: List[BlackboardItem[T, _]] = List()
  override def toJson(prefix: List[String], b: B)(implicit bundle: ResourceBundle): JsonValue = using(lens(b)) { t =>
    JsonObject("leaf" -> JsonObject(
      "id" -> (prefix :+ name).mkString("."),
      "name" -> bundle.getString((prefix :+ name).mkString(".")),
      "value" -> stringLens(t),
      "validation" -> JsonList(validateFn(t).map(v => JsonString(v.s)))))
  }
  override def findLens(name: List[String]): Lens[T, String] = if (name.isEmpty) stringLens else throw new RuntimeException(s"Cannot process name [${name}]")
}

case class NestedBlackboard[B, T](blackboard: Blackboard[T], lens: Lens[B, T]) extends BlackboardItem[B, T] {
  override def name: String = blackboard.name
  override def children: List[BlackboardItem[T, _]] = blackboard.children
  override def validateFn: T => List[ValidateProblem] = t => children.flatMap(_.validate(t))
  override def toJson(prefix: List[String], b: B)(implicit bundle: ResourceBundle): JsonValue = using(lens(b)) { t => JsonObject("name" -> bundle.getString((("title" :: prefix) :+ name).mkString(".")), "children" -> JsonList(children.map(_.toJson(prefix :+ name, t)))) }
  override def toString: String = s"Blackboard(${blackboard.name})"
  override def findLens(name: List[String]): Lens[T, String] = blackboard.findLens(name)

}

class SimpleBlackboard[B](val name: String, var children: List[BlackboardItem[B, _]] = List()) extends Blackboard[B] {
  case class question[T](name: String) {
    case class getter(g: B => T)(implicit stringLens: Lens[T, String]) {
      case class setter(s: (B, T) => B) {
        def validate(fn: T => List[ValidateProblem]) =
          SimpleBlackboardItem(name, Lens[B, T](g, s), fn) sideeffect (i => children = children :+ i)
      }
    }
  }
  case class child[T](item: Blackboard[T]) {
    case class getter(g: B => T) {
      def setter(s: (B, T) => B) = NestedBlackboard(item, Lens(g, s)) sideeffect (i => children = children :+ i)

    }
  }
  def toJson(b: B)(implicit bundle: ResourceBundle) = JsonList(children.map(_.toJson(List(), b)))
  def validate(b: B) = children.flatMap(_.validate(b))
  override def findLens(name: List[String]): Lens[B, String] = name match {
    case head :: tail => children.find(_.name == head).fold(throw new RuntimeException(s"could could not find [$name]")) { child => child.lensAndFindLens(tail) }
    case _ => throw new RuntimeException("could not find for Nil")
  }

  def update(e: B)(list: List[(String, String)]): B = {
    list.foldLeft(e) { case (acc, (name, value)) =>
      val lens: Lens[B, String] = findLens(name.split("\\.").filterNot(_.isEmpty).toList)
      lens.set(acc, value)
    }
  }
}

trait Question {
  def isDefined(s: String) = if (s.isEmpty) List(ValidateProblem("Required")) else List()
  def isNonZero(g: GBP) = if (g.amnt == 0) List(ValidateProblem("Required")) else List()
  def allgood[S](s: S) = List()

  implicit val addressStringL = Lens[Address, String](_.s, (a, s) => a.copy(s = s))
  implicit val gbpToStringL = Lens[GBP, String](_.amnt.toString, (a, s) => a.copy(amnt = s.toInt))

  val identity = new SimpleBlackboard[Identity]("identity") {
    val nameF = question[String]("name") getter (_.name) setter ((i, n) => i.copy(name = n)) validate isDefined
    val opAddressF = question[Address]("operationAddress") getter (_.operationAddress) setter ((i, n) => i.copy(operationAddress = n)) validate ({ a: Address => a.s } andThen isDefined)
    val regAddressF = question[Address]("registeredAddress") getter (_.registeredAddress) setter ((i, n) => i.copy(registeredAddress = n)) validate allgood
  }

  val balanceSheet = new SimpleBlackboard[BalanceSheet]("balanceSheet") {
    val totalNetAssetsF = question[GBP]("totalNetAssets") getter (_.totalNetAssets) setter ((i, n) => i.copy(totalNetAssets = n)) validate isNonZero
    val totalLiabilitiesF = question[GBP]("totalLiabilities") getter (_.totalLiabilities) setter ((i, n) => i.copy(totalLiabilities = n)) validate isNonZero
    val shareHoldersInterestF = question[GBP]("shareHoldersInterest") getter (_.shareHoldersInterest) setter ((i, n) => i.copy(shareHoldersInterest = n)) validate isNonZero
  }
  val profitAndLoss = new SimpleBlackboard[ProfitAndLoss]("profitAndLost") {
    val nettExpenditureF = question[GBP]("nettExpenditure") getter (_.nettExpenditure) setter ((i, n) => i.copy(nettExpenditure = n)) validate isNonZero
    val nettIncomeF = question[GBP]("nettIncome") getter (_.nettIncome) setter ((i, n) => i.copy(nettIncome = n)) validate isNonZero
  }

  val financialData = new SimpleBlackboard[FinancialData]("financialData") {
    val balanceSheetF = child(balanceSheet) getter (_.balanceSheet) setter ((a, b) => a.copy(balanceSheet = b))
    val profitAndLossF = child(profitAndLoss) getter (_.profitAndLoss) setter ((a, b) => a.copy(profitAndLoss = b))
  }

  implicit val blackboard = new SimpleBlackboard[Entity]("entity") {
    val identityF = child(identity) getter (_.identity) setter ((e, i) => e.copy(identity = i))
    val financialDataF = child(financialData) getter (_.financialData) setter ((e, i) => e.copy(financialData = i))
  }


  val decision: Entity => Option[(BlackboardItem[Entity, _], String)] = Decision(blackboard.identityF -> "entity.identity", blackboard.financialDataF -> "identity.financialData")

  val entity = Entity(
    Identity("UBS", IndustrySector("something"), Address(""), Address(""), TeleNo("phoneNo"), TeleNo("fax"), Website(""), BIC("someBic"), Chaps("someChaps"), ClearingCode("someClearingCode")),
    FinancialData(BalanceSheet(totalNetAssets = GBP(123), totalLiabilities = GBP(234), shareHoldersInterest = GBP(0)), ProfitAndLoss(nettIncome = GBP(12323), nettExpenditure = GBP(234))),
    Activities("someMainActivies", "someProductAndServices", "someBuisnessLine", Naics("someNaicsCode"), Nace(144)),
    GateKeeperThings(false)
  )

}

object Question extends Question with App {
  implicit val resource = ResourceBundle.getBundle("message")
  //  println(blackboard.toJson(entity))
  import one.xingyi.json4s.Json4sWriter._
  println(implicitly[JsonWriter[JValue]].apply(blackboard.toJson(entity)))
  println(decision(entity))
  println
  println("validation")
  println(blackboard.validate(entity))


  println(blackboard.findLens(List("identity", "name"))(entity))
  println(blackboard.findLens(List("financialData", "balanceSheet", "totalNetAssets"))(entity))
  val newEntity = blackboard.update(entity)(List("financialData.balanceSheet.totalNetAssets" -> "1234", "identity.name" -> "newName"))
  println(implicitly[JsonWriter[JValue]].apply(blackboard.toJson(newEntity)))

}


object Website extends App with Question {
  implicit val resource = ResourceBundle.getBundle("message")
  import Json4sWriter._
  implicit def template: MustacheWithTemplate[JValue] = Mustache.withTemplate("main.template.mustache") apply("question.mustache", "Coolness")
  val executor = HttpUtils.makeDefaultExecutor
  var theEntity = entity
  def html = {
    val dec = decision(theEntity) match {
      case None => JsonString("none")
      case Some((entity, name)) => JsonObject("name" -> name, "val" -> entity.toJson(List(), theEntity))
    }
    template.apply(JsonMaps(JsonObject("question" -> dec, "entity" -> blackboard.toJson(theEntity))))
  }
  new SimpleHttpServer(9000, executor,
    new PathAndHandler {
      override def path(): String = "/change"
      override def handle(httpExchange: HttpExchange): Unit = {
        try {
          val x = httpExchange.getRequestURI.getQuery.split("&").toList.flatMap(s => s.split('=').toList match {
            case left :: right :: _ => List(left -> right)
            case left :: _ => List(left -> "")
            case _ => List()
          })
          println(x)
          theEntity = blackboard.update(theEntity)(x)
          HttpUtils.process(httpExchange, () => new SimpleHttpResponse(200, "text/html", html))
        } catch {case e => e.printStackTrace(); HttpUtils.process(httpExchange, () => new SimpleHttpResponse(200, "text/html", s"Error: $e"))}
      }
    },
    new PathAndHandler {
      override def path(): String = "/"
      override def handle(httpExchange: HttpExchange): Unit = {

        theEntity = entity

        HttpUtils.process(httpExchange, () => new SimpleHttpResponse(200, "text/html", html))
      }
    }
  ).start()

}


//    def nameQ
//
//
//    def nameQuestion: Question[String] = name => JsonObject("questionText" -> "Name", "formType" -> "text", "value" -> name)
//    def addressQuestion(addressName: String): Question[String] = address => JsonObject("questionText" -> address, "formType" -> "text", "value" -> address)
//    def teleNoQuestion: Question[String] = address => JsonObject("questionText" -> "Telephone No", "formType" -> "text", "value" -> address)
//    def faxNoQuestion: Question[String] = address => JsonObject("questionText" -> "Fax No", "formType" -> "text", "value" -> address)
//    def bicsQuestion: Question[String] = address => JsonObject("questionText" -> "Fax No", "formType" -> "text", "value" -> address)
//    def chapsQuestion: Question[String] = address => JsonObject("questionText" -> "Fax No", "formType" -> "text", "value" -> address)
//    def clearingCodeQuestion: Question[String] = address => JsonObject("questionText" -> "Fax No", "formType" -> "text", "value" -> address)
//
//    def identityQuestion: Question[Identity] = identity => section(
//      nameQuestion,
//      addressQuestion,
//      teleNoQuestion,
//      faxNoQuestion,
//      bicsQuestion,
//      chapsQuestion,
//      clearingCodeQuestion
//    )
//
//    def totalNetAssetsQ: Question[GBP] = assets => JsonObject("questionText" -> "What are the total net assets", "formType" -> "text", "value" -> assets)
//    def totalLiabilities: Question[GBP] = totalLiabilities => JsonObject("questionText" -> "What are the totalLiabilities", "formType" -> "text", "value" -> totalLiabilities)
//    def shareHoldersQ: Question[GBP] = shareHoldersInterest => JsonObject("questionText" -> "What are the totalLiabilities", "formType" -> "text", "value" -> totalLiabilities)
//    def balanceSheetQ: Question[BalanceSheet] = section(totalNetAssetsQ, totalLiabilities, shareHoldersQ)
//
//
//    def netIncomeQ: Question[GBP] = nett => JsonObject("questionText" -> "What is the net income", "formType" -> "text", "value" -> nett.amnt)
//    def netExpenditure: Question[GBP] = nett => JsonObject("questionText" -> "What is the net expenditure", "formType" -> "text", "value" -> nett.amnt)
//    def profileAndLossQuestiob: Question[ProfitAndLoss] = section(netIncomeQ, netExpenditure)
//    def financeQuestion: Question[FinancialData] = section(balanceSheetQ, profileAndLossQuestiob)
//    def gateKeeperQuestion: Question[GateKeeperThings] = ??? //section(balanceSheetQ, profileAndLossQuestiob)


//  def identityQuestion[J]: Question[J, Identity] = JsonObject("children" -> JsonList(List(


