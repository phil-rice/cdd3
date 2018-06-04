package one.xingyi.cddcore


case class ScenarioLogic[P, R](result: Option[R], definedAt: Option[P => Boolean], code: Option[P => R]) {
  def acceptResult(r: R) = result.fold(true)(_ == r)
  def isDefinedAt(p: P): Boolean = definedAt.fold(true)(d => d(p))
  def applyOr(p: P, default: => R): R = code.fold(default)(code => code(p))
  def partialFunction(default: P=> R) : PartialFunction[P, R]= {case p if isDefinedAt(p) => applyOr(p, default(p))}
}

