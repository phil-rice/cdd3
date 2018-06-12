package one.xingyi.cddutilities

sealed trait TestResult
case object TestSucceeded extends TestResult
case object TestFailed extends TestResult

sealed trait EngineTest
case class ScenarioTest(name: String, testResult: TestResult) extends EngineTest
case class NestedTest(name: String, tests: Seq[EngineTest]) extends EngineTest
trait SaveTest[J] extends (NestedTest => J)
