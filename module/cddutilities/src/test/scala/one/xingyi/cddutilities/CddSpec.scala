/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddutilities

import one.xingyi.cddutilities.strings.Strings
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}

trait CddSpec extends FlatSpec with Matchers with MockitoSugar {
  def safeMake[X](x: => X) = try {
    x
  } catch {
    case e: Exception => e.printStackTrace(); throw e
  }

  def bigStringsShouldBeEqual(expected: String)(actual: String) =
    withClue("Expected is\n" + expected + "\nActual is\n" + actual + "\n")(
      Strings.splitLines(expected).zip(Strings.splitLines(actual)).zipWithIndex.foreach {
        case ((expectedLine, actualLine), i) =>
        withClue(s"Line $i\n")(expectedLine shouldBe actualLine)
      })

  def listShouldBeEqualTo[X](actual: List[X])(expected: X*): Unit =
    withClue("Expected is\n" + expected + "\nActual is\n" + actual + "\n") {
      expected.zip(actual).zipWithIndex.foreach {
        case ((expectedItem, actualItem), i) =>
        withClue(s"Actual Item($i)\n$actualItem\nExpected Item($i)\n$expectedItem")(actualItem shouldBe expectedItem)
      }
      actual.size shouldBe expected.size
    }

  implicit class StringOps(s: String) {
    def noWhiteSpace = s.replaceAll("\\se*", "")
  }
}
