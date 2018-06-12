package one.xingyi.cddutilities

sealed trait TestResult
case object TestSucceeded extends TestResult
case object TestFailed extends TestResult

trait TestFramework[J]{
  def createTest(t: NestedTest): J
}

sealed trait EngineTest
case class ScenarioTest(name: String, block: ()=> Unit) extends EngineTest
case class NestedTest(name: String, tests: Seq[EngineTest]) extends EngineTest
