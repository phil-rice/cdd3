package one.xingyi.cddcore
import one.xingyi.cddutilities.{DefinedInSourceCodeAt, ShortPrint}

case class NoDefaultDefinedException(p: Any) extends RuntimeException(s"Calling apply using $p without a default defined")

object ScenarioLogic {
  def empty[P, R] = SingleScenarioLogic[P, R](None, None, None, DefinedInSourceCodeAt.definedInSourceCodeAt(1))
}

case class UndefinedException(logic: Seq[PartialFunction[_, _]], p: Any) extends RuntimeException(s"Cannot execute because undefined at $p. \nLogic is\n${logic.mkString("\n")}")

trait ScenarioLogic[P, R] extends PartialFunction[P, R] {
  def accept(s: Scenario[P, R]) = isDefinedAt(s.situation) && s.acceptResult(s.situation, apply(s.situation))

  def or(logic: SingleScenarioLogic[P, R]): ScenarioLogic[P, R]

  def hasCondition: Boolean
}
case class SingleScenarioLogic[P, R](result: Option[R], definedAt: Option[P => Boolean], code: Option[P => R], definedInSourceCodeAt: DefinedInSourceCodeAt)(implicit shortPrintP: ShortPrint[P], shortPrintR: ShortPrint[R]) extends ScenarioLogic[P, R] {
  override def hasCondition: Boolean = definedAt.isDefined
  private def resultAsFn = result.map(r => { p: P => r })
  def acceptResult(r: R) = result.fold(true)(_ == r)
  override def apply(p: P): R = applyOr(_ => throw new NoDefaultDefinedException(p))(p)
  def isDefinedAt(p: P): Boolean = definedAt.fold(result.isDefined || code.isDefined)(d => d(p))
  def applyOr(default: P => R): P => R = code orElse resultAsFn getOrElse default
  override def toString(): String = s"SLogic(${result.map(shortPrintR)},$definedInSourceCodeAt)"
  override def or(logic: SingleScenarioLogic[P, R]): ScenarioLogic[P, R] = CompositeScenarioLogic(Seq(this, logic))
}

case class CompositeScenarioLogic[P, R](logics: Seq[ScenarioLogic[P, R]]) extends ScenarioLogic[P, R] {
  override def isDefinedAt(p: P): Boolean = logics.exists(_.isDefinedAt(p))
  override def apply(p: P): R = logics.find(_.isDefinedAt(p)).getOrElse(throw new UndefinedException(logics, p)).apply(p)
  override def hasCondition: Boolean = logics.exists(_.hasCondition)
  override def or(logic: SingleScenarioLogic[P, R]): ScenarioLogic[P, R] = CompositeScenarioLogic(logics :+ logic)
  override def toString(): String = s"SCompLogic(${logics.mkString(",")})"
}

case class Scenario[P, R](situation: P, logic: SingleScenarioLogic[P, R], assertions: List[ScenarioAssertion[P, R]], data: EngineComponentData) extends EngineComponent[P, R] {
  def acceptResult(p: P, r: R) = logic.result.fold(true)(_ == r) && assertions.forall(_.isTrue(p, r))
}


