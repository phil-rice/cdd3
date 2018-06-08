package one.xingyi.cddscenario

import scala.language.higherKinds

trait HasScenarios[T[_, _]] {
  def allScenarios[P, R](t: T[P, R]): List[Scenario[P, R]]
}
object NoDefaultDefinedException {
  def throwWith[P]: PartialFunction[P, Nothing] = {case p if false => throw new NoDefaultDefinedException(p)}
}
case class NoDefaultDefinedException(p: Any) extends RuntimeException(s"Calling apply using $p without a default defined")

case class UndefinedException(logic: Seq[PartialFunction[_, _]], p: Any) extends RuntimeException(s"Cannot execute because undefined at $p. \nLogic is\n${logic.mkString("\n")}")

case class Scenario[P, R](situation: P, logic: SingleScenarioLogic[P, R], assertions: List[ScenarioAssertion[P, R]], data: EngineComponentData) extends EngineComponent[P, R] {
  def acceptResult(p: P, r: R) = logic.result.fold(true)(_ == r) && assertions.forall(_.isTrue(p, r))
}


