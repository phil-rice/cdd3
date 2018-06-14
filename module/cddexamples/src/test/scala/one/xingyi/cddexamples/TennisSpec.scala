package one.xingyi.cddexamples
import one.xingyi.cddutilities.CddSpec



class TennisSpec extends CddSpec {


  behavior of "Tennis"

  it should "work" in {
    val t = new Tennis()
    import t._
    dump
     tennis(1, 1) shouldBe "fifteen all"
    tennis(2, 1) shouldBe "thirty, fifteen"
    tennis(3, 1) shouldBe "forty, fifteen"
    tennis(3, 2) shouldBe "forty, thirty"
    tennis(3, 4) shouldBe "advantage right"
    tennis(4, 4) shouldBe "deuce"
    tennis(4, 5) shouldBe "advantage right"
    tennis(5, 5) shouldBe "deuce"
    tennis(6, 5) shouldBe "advantage left"
    tennis(7, 5) shouldBe "left won"

    tennis(3, 3) shouldBe "deuce"
  }

}
