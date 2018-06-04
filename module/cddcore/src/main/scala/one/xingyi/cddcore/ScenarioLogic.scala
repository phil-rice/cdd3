package one.xingyi.cddcore



sealed trait ScenarioLogic[P, R] {
  def code: Option[P => R]
  def isDefinedAt: P => Boolean
  def result: Option[R]
  def acceptResult(p: P, r: R): Boolean = Some(r) == result
  def partialFn: PartialFunction[P,R]
}
case class NoLogic[P, R](result: Option[R]) extends ScenarioLogic[P, R] {
  override def isDefinedAt = _ => true
  override def code: Option[P => R] = None
  override def partialFn: PartialFunction[P, R] = code.fold[PartialFunction[P,R]](throw new IllegalStateException(s"Cannot make partial function when NoLogic"))(c=> {case p => c(p)})
}
case class WithWhenLogic[P, R](result: Option[R], isDefinedAt: P => Boolean, code: Option[P => R]) extends ScenarioLogic[P, R] {
  override def partialFn: PartialFunction[P, R] = code.fold[PartialFunction[P,R]](throw new IllegalStateException(s"Cannot make partial function when NoLogic")){c => {case p if isDefinedAt(p) => c(p)}}
}
case class WithBecauseLogic[P, R](result: Option[R], because: PartialFunction[P, R]) extends ScenarioLogic[P, R] {
  override def isDefinedAt = because.isDefinedAt
  override def code: Option[P => R] = Some(because)
  override def partialFn: PartialFunction[P, R] = because
}
