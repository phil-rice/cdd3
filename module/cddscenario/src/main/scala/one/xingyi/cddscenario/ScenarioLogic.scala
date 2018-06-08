package one.xingyi.cddscenario
import one.xingyi.cddutilities._
import PartialFunctions._
import one.xingyi.cddutilities.SemiGroupLanguage._

trait HasCondition[T] extends (T => Boolean)

object ScenarioLogic {
  def empty[P, R] = SingleScenarioLogic[P, R](None, None, None, DefinedInSourceCodeAt.definedInSourceCodeAt(2))
  def toSeq[P, R]: ScenarioLogic[P, R] => Seq[SingleScenarioLogic[P, R]] = {case s: SingleScenarioLogic[P, R] => List(s); case CompositeScenarioLogic(defined) => defined}
  implicit def semiGroupForScenarioLogic[P, R]: SemiGroup[ScenarioLogic[P, R]] = (t1, t2) => CompositeScenarioLogic(toSeq(t1) ++ toSeq(t2))
  implicit def isDefinedInSourceCodeAt[P, R]: IsDefinedInSourceCodeAt[ScenarioLogic[P, R]] = {
    case s: SingleScenarioLogic[P, R] => s.definedInSourceCodeAt
    case CompositeScenarioLogic(logics) => CompositeDefinedInSourceCodeAt(logics.map(_.definedInSourceCodeAt))
  }


  trait ScenarioLogic[P, R] {
    def fn: PartialFunction[P, R]
    def hasCondition: Boolean
    def accept(s: Scenario[P, R]) = fn.isDefinedAt(s.situation) && s.acceptResult(s.situation, fn.apply(s.situation))

  }

  object SingleScenarioLogic {
    implicit def slAsHasCondition[P, R]: HasCondition[SingleScenarioLogic[P, R]] = {_.definedAt.isDefined}
  }

  case class SingleScenarioLogic[P, R](result: Option[R], definedAt: Option[P => Boolean], code: Option[P => R], definedInSourceCodeAt: SingleDefinedInSourceCodeAt)(implicit shortPrintP: ShortPrint[P], shortPrintR: ShortPrint[R]) extends ScenarioLogic[P, R] {
    val fn = (code or result.asFun) asPFn definedAt getOrElse NoDefaultDefinedException.throwWith[P]
    override def hasCondition: Boolean = definedAt.isDefined
    def acceptResult(r: R) = result.fold(true)(_ == r)
    //    override def apply(p: P): R = code or result.asFun getOrElse NoDefaultDefinedException.throwWith apply p
    def isDefinedAt(p: P): Boolean = definedAt.fold(result.isDefined || code.isDefined)(d => d(p))

    override def toString(): String = s"SLogic(${result.map(shortPrintR)},$definedInSourceCodeAt)"
  }

  object CompositeScenarioLogic {
    implicit def slAsHasCondition[P, R](implicit slHasCond: HasCondition[ScenarioLogic[P, R]]): HasCondition[CompositeScenarioLogic[P, R]] = {_.logics.exists(slHasCond)}
  }

  case class CompositeScenarioLogic[P, R](logics: Seq[SingleScenarioLogic[P, R]]) extends ScenarioLogic[P, R] {
    import SemiGroupLanguage._
    //    override def apply(p: P): R = logics.find(_.isDefinedAt(p)).getOrElse(throw new UndefinedException(logics, p)).apply(p)
    override def hasCondition: Boolean = logics.exists(_.hasCondition)
    //    override def isDefinedAt(p: P): Boolean = logics.exists(_.isDefinedAt(p))
    override def toString(): String = s"SCompLogic(${logics.mkString(",")})"
    override def fn: PartialFunction[P, R] = logics.map(_.fn).orAll
  }
}