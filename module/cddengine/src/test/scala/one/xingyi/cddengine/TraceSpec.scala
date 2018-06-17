package one.xingyi.cddengine
import one.xingyi.cddutilities.CddSpec
import org.scalatest.mockito.MockitoSugar
class TraceSpec extends CddSpec with EngineFixture  with MockitoSugar{

  behavior of "enginetools.trace"

  it should "make a list of trace data" ignore {
    //ok clearly too hard to test! Need to simplify
//    val printWriter = mock[PrintWriter]
//    implicit val printRenderToFile = mock[PrintRenderToFile]
//    e.tools.trace("somePrefix")
  }
}
