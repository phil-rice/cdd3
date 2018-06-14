package one.xingyi.cddexamples
import one.xingyi.cddengine.Engine
import one.xingyi.cddscalatest.CddFlatSpec

import scala.util.Try

class TennisCddRunner extends CddFlatSpec {
  override protected def engines: Try[List[Engine[_, _]]] = Try(List(new Tennis().tennis))

}
