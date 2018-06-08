package one.xingyi.cddscenario
import one.xingyi.cddutilities.{DefinedInSourceCodeAt, SingleDefinedInSourceCodeAt}

case class ScenarioBuilderData[P, R](id: Int, situation: P, result: Option[R] = None, title: Option[String] = None, comment: Option[String] = None, assertions: List[ScenarioAssertion[P, R]] = List(),
                                     references: List[Reference] = List(), isDefinedAt: SingleDefinedInSourceCodeAt = DefinedInSourceCodeAt.definedInSourceCodeAt()) {
  def data: EngineComponentData = EngineComponentData(isDefinedAt, title, comment, List(), references)
}
