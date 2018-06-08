package one.xingyi.cddcore
import one.xingyi.cddscenario.ScenarioLogic.ScenarioLogic
import one.xingyi.cddscenario.{HasScenarios, Scenario, ScenarioLogic}
import one.xingyi.cddutilities.DefinedInSourceCodeAt

import scala.language.higherKinds

trait Engine[P, R] extends PartialFunction[P, R]

case class Engine1[P, R](scenarios: List[Scenario[P, R]])(implicit dtFolder: DecisionTreeFolder[P, R]) extends Engine[P, R] {
  val dt = scenarios.foldLeft(DecisionTree.empty[P, R])(dtFolder)
  def logicFor(p: P): ScenarioLogic[P, R] = dt.root.findLens(p).get(dt.root).logic
  override def isDefinedAt(p: P): Boolean = logicFor(p).fn.isDefinedAt(p)
  override def apply(p: P): R = logicFor(p).fn apply p //later we can be more efficient. Don't optimise just yet
}

object Engine {
  def apply[T[_, _], P, R](t: T[P, R])(implicit hasScenarios: HasScenarios[T]): Engine[P, R] = Engine1(hasScenarios.allScenarios[P, R](t))
//  def apply[P, R](title: String)(block: ScenarioAggregator[P, R] => Unit): Engine[P, R] = Engine1(UseCase(title)(block).allScenarios)

}
