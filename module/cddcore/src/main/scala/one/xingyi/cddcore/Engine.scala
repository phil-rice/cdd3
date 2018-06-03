package one.xingyi.cddcore
import one.xingyi.cddutilities.DefinedInSourceCodeAt

trait Engine[P, R] extends PartialFunction[P, R]

case class Engine1[P, R](useCase: UseCase[P, R])(implicit dtFolder: DecisionTreeFolder[P, R]) extends Engine[P, R] {
  val dt = useCase.allScenarios.foldLeft(DecisionTree.empty[P, R])(dtFolder)
  def logicFor(p: P): ScenarioLogic[P, R] = dt.root.findLens(p).get(dt.root).logic
  override def isDefinedAt(p: P): Boolean = logicFor(p).isDefinedAt(p)
  override def apply(p: P): R = logicFor(p) apply p //later we can be more efficient. Don't optimise just yet
}

object Engine {

  def apply[P, R](title: String)(block: ScenarioAggregator[P, R] => Unit): Engine[P, R] = Engine1(UseCase(title)(block))

}
