package one.xingyi.cddscalatest
import one.xingyi.cddengine.CddRunner
import one.xingyi.cddutilities.{CddTest, NestedTest, ScenarioTest, TestFramework}
import org.scalatest.FunSpecLike

class ScalaTestFramework extends TestFramework[Unit] {
  override def apply(v1: CddTest): Unit = ???
}


trait CddFlatSpec extends FunSpecLike with CddRunner {

  println(tryTests)
  tryTests.get.tests.foreach(build)

  def build(test: CddTest): Unit = test match {
    case s: ScenarioTest => it(s.name)(s.block())
    case n: NestedTest => describe(n.name)(n.tests.foreach(build))
  }
}