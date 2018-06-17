package one.xingyi.cddengine
import one.xingyi.cddutilities.CddSpec

class DefaultFunctionSpec extends CddSpec {

  behavior of "SimpleDefaultFunction"

  val f = new SimpleDefaultFunction[Int, Int](_ + 1)

  it should "be defined anywhere" in {
    f.isDefinedAt(1) shouldBe true
    f.isDefinedAt(-1) shouldBe true
  }

  it should "executed the passed in function" in {
    f(1) shouldBe 2
  }
}
