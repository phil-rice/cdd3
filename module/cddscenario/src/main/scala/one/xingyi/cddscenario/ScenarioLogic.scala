package one.xingyi.cddscenario
import one.xingyi.cddutilities._
import PartialFunctions._
import one.xingyi.cddutilities.SemiGroupLanguage._


object ScenarioLogic {
  def empty[P, R] = SingleScenarioLogic[P, R](None, None, None, DefinedInSourceCodeAt.definedInSourceCodeAt(2), "<empty>")
  def toSeq[P, R]: ScenarioLogic[P, R] => Seq[SingleScenarioLogic[P, R]] = {case s: SingleScenarioLogic[P, R] => List(s); case CompositeScenarioLogic(defined) => defined}
  implicit def semiGroupForScenarioLogic[P, R]: SemiGroup[ScenarioLogic[P, R]] = (t1, t2) => CompositeScenarioLogic(toSeq(t1) ++ toSeq(t2))
  implicit def isDefinedInSourceCodeAt[P, R]: IsDefinedInSourceCodeAt[ScenarioLogic[P, R]] = {
    case s: SingleScenarioLogic[P, R] => s.definedInSourceCodeAt
    case CompositeScenarioLogic(logics) => CompositeDefinedInSourceCodeAt(logics.map(_.definedInSourceCodeAt))
  }

}
trait ScenarioLogic[P, R] {
  def ifString: String
  def fn: PartialFunction[P, R]
  def hasCondition: Boolean
  def accept(s: Scenario[P, R]) = fn.isDefinedAt(s.situation) && s.acceptResult(s.situation, fn.apply(s.situation))
}


case class SingleScenarioLogic[P, R](result: Option[R], definedAt: Option[P => Boolean], code: Option[P => R], definedInSourceCodeAt: SingleDefinedInSourceCodeAt, ifString: String)(implicit shortPrintP: ShortPrint[P], shortPrintR: ShortPrint[R]) extends ScenarioLogic[P, R] {
  val fn = (code or result.asFun) asPFn definedAt getOrElse NoDefaultDefinedException.throwWith[P]
  override def hasCondition: Boolean = definedAt.isDefined
  override def toString(): String = s"SLogic(${result.map(shortPrintR)},$definedInSourceCodeAt)"
}


case class CompositeScenarioLogic[P, R](logics: Seq[SingleScenarioLogic[P, R]]) extends ScenarioLogic[P, R] {
  import SemiGroupLanguage._
  override def hasCondition: Boolean = logics.exists(_.hasCondition)
  override val fn: PartialFunction[P, R] = logics.map(_.fn).orAll
  override def toString(): String = s"SCompLogic(${logics.mkString(",")})"
  override def ifString: String = logics.map(_.ifString).mkString(" or ")
}
