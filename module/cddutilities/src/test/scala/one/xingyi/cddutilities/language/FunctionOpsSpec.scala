/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddutilities.language
import java.util.concurrent.atomic.AtomicReference

import one.xingyi.cddutilities.CddSpec
import org.scalatest.Matchers
trait FunctionFixture extends Matchers {
  def fn[X, Y](expected: X, y: => Y) = { x: X => x shouldBe expected; y }
  def fn2[X, Y, Z](expectedX: X, expectedY: Y, z: => Z) = { (x: X, y: Y) => x shouldBe expectedX; y shouldBe expectedY; z }
  def fn2Curried[X, Y, Z](expectedX: => X, expectedY: => Y, z: => Z) = { x: X => y: Y => x shouldBe expectedX; y shouldBe expectedY; println(s"fn2 $expectedX, $expectedY, $z"); z }
  def sideeffect[X](atomicReference: AtomicReference[X]): X => Unit = atomicReference.set _

}

class FunctionOpsSpec extends CddSpec with FunctionFixture with FunctionLanguage {

  behavior of "FunctionFromMidToOptionOps"

  it should "allow a 'orElse' " in {
    (fn2Curried(1, 2, Some(3)) orElse fn2Curried(throw new RuntimeException("1"), throw new RuntimeException("2"), throw new RuntimeException("3"))) (1)(2) shouldBe Some(3)
    (fn2Curried(1, 2, None) orElse fn2Curried(1, 2, Some(3))) (1)(2) shouldBe Some(3)
  }
  it should "allow a 'orDefault' " in {
    (fn2Curried(1, 2, Some(3)) orDefault (throw new RuntimeException("1"))) (1)(2) shouldBe 3
    (fn2Curried(1, 2, None) orDefault (3)) (1)(2) shouldBe 3

  }
}
