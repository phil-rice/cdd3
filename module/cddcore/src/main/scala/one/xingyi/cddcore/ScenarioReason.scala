package one.xingyi.cddcore

sealed trait ScenarioReason[P, R] {
  def result: Option[R]
  def acceptResult(p: P, r: R): Boolean = Some(r) == result
}
case class NoReason[P, R](result: Option[R]) extends ScenarioReason[P, R]
case class WithWhenReason[P, R](result: Option[R], when: P => Boolean) extends ScenarioReason[P, R]
case class WithBecauseReason[P, R](result: Option[R], because: PartialFunction[P, R]) extends ScenarioReason[P, R]
