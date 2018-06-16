package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario

sealed trait ValidationIssues[P, R]
case class ScenarioComesToWrongConclusion[P, R](s: Scenario[P, R], actualResult: R) extends ValidationIssues[P, R]

case class ValidationReport[P, R](engine: Engine[P, R], issues: List[ValidationIssues[P, R]])
trait Validation[P, R] extends (Engine[P, R] => ValidationReport[P, R])

class SimpleValidation[P, R] extends Validation[P, R] {
  override def apply(e: Engine[P, R]): ValidationReport[P, R] =
    ValidationReport(e, e.tools.scenarios.map(s => (s, e.apply(s.situation))).collect { case (s, r) if s.result.isDefined && Some(r) != s.result =>
      val result = ScenarioComesToWrongConclusion(s, r)
      println(s" s is [${s.situation}]\n Actual is    $r\nExpected is ${s.result} result: $result")
      result
    })


}
