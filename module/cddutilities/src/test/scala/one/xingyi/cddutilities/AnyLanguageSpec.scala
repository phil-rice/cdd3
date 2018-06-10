package one.xingyi.cddutilities

class AnyLanguageSpec extends CddSpec with AnyLanguage {

  behavior of "AnyOps"

  it should "Have a using method" in {
    using(1)(_ + 2) shouldBe 3
  }
}
