package one.xingyi.cddcore

trait CddIssue
trait ScenarioAssertion[P, R] extends ((P, R) => Either[CddIssue, Boolean]){
  def isTrue(p: P, r: R) = apply(p,r).getOrElse(false)
}
