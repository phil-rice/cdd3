package one.xingyi.cddexamples
import one.xingyi.cddengine.Engine
import one.xingyi.cddunit.NewCddRunner

import scala.util.Try

class TennisCddRunner extends NewCddRunner {
  override protected def engines: Try[List[Engine[_, _]]] = Try(List(new Tennis().tennis))
}
