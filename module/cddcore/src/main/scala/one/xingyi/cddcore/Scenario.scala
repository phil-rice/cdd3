package one.xingyi.cddcore


case class Scenario[P, R](situation: P, reason: ScenarioReason[P, R], assertions: List[ScenarioAssertion[P, R]], data: EngineComponentData) extends EngineComponent[P, R] {
  def acceptResult(p: P, r: R) = reason.result.fold(true)(_ == r) && assertions.forall(_.isTrue(p, r))
}


