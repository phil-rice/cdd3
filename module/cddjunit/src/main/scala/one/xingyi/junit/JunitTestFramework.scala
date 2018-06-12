package one.xingyi.junit
import junit.framework.TestSuite
import one.xingyi.cddutilities.{NestedTest, ScenarioTest, TestFramework}
import org.junit.runners.model.TestClass

class JunitTestFramework extends TestFramework[TestSuite] {

  override def createTest(t: NestedTest): TestSuite = {
    val ts = new TestSuite(t.name)
    t.tests.foreach {
      case t: NestedTest => ts.addTest(createTest(t))
      case s: ScenarioTest =>
         ts.ad
    }

  }
}
