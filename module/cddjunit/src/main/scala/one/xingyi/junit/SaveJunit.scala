package one.xingyi.junit
import junit.framework.TestSuite
import one.xingyi.cddutilities.{EngineTest, NestedTest, SaveTest}

class SaveJunit extends SaveTest[TestSuite] {

  override def apply(e: NestedTest): TestSuite = {
    ???
  }
}
