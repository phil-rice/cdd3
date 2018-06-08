package one.xingyi.cddscenario

abstract class ScenarioBuilderComponent[Self <: ScenarioBuilderComponent[Self, P, R], P, R](implicit a: ScenarioAggregator[P, R]) {
  a.apply(this)
  import one.xingyi.cddutilities.Arrows._

  def scenarioReason: SingleScenarioLogic[P, R]
  def scenario: Scenario[P, R] = Scenario(data.situation, scenarioReason, data.assertions, data.data)
  def data: ScenarioBuilderData[P, R]
  protected def copyWith(fn: ScenarioBuilderData[P, R] => ScenarioBuilderData[P, R])(implicit a: ScenarioAggregator[P, R]): Self = rawCopyWith _ sideeffect a apply fn
  protected def rawCopyWith(fn: ScenarioBuilderData[P, R] => ScenarioBuilderData[P, R]): Self
  def comment(string: String): Self = copyWith(_.copy(comment = Some(string)))
  def title(string: String): Self = copyWith(_.copy(title = Some(string)))
  def reference(reference: Reference): Self = copyWith(d => d.copy(references = d.references :+ reference))
}
