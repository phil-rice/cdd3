/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddutilities

class StringsSpec extends CddSpec {

  "The Strings.findFirst" should "extract the first chunk that matches the criteria " in {
    Strings.findFirst("from", "to")("fromheretohere") shouldBe Some("here")
    Strings.findFirst("from", "to")("asdfromheretohere") shouldBe Some("here")
    Strings.findFirst("from", "to")("sdfromheretofromhere") shouldBe Some("here")
    Strings.findFirst("from", "to")("") shouldBe None
    Strings.findFirst("", "")("asd") shouldBe Some("")
    Strings.findFirst("", "a")("asd") shouldBe Some("")
  }

  "The Strings.findFirstIncludingPrefix" should "extract the first chunk that matches the criteria " in {
    Strings.findFirstIncludingPrefix("from", "to")("fromheretohere") shouldBe Some("fromhere")
    Strings.findFirstIncludingPrefix("from", "to")("fromheretofromhere") shouldBe Some("fromhere")
    Strings.findFirstIncludingPrefix("from", "to")("asdfromheretohere") shouldBe Some("fromhere")
    Strings.findFirstIncludingPrefix("from", "to")("sdffromheretofromhere") shouldBe Some("fromhere")
    Strings.findFirstIncludingPrefix("from", "to")("") shouldBe None
    Strings.findFirstIncludingPrefix("", "")("asd") shouldBe Some("")
    Strings.findFirstIncludingPrefix("", "a")("asd") shouldBe Some("")
  }

  "The Strings.from" should "extract the first chunk that matches the criteria " in {
    Strings.from("from")("fromheretohere") shouldBe Some("fromheretohere")
    Strings.from("from")("fromheretofromhere") shouldBe Some("fromheretofromhere")
    Strings.from("from")("asdfromheretohere") shouldBe Some("fromheretohere")
    Strings.from("from")("sdffromheretofromhere") shouldBe Some("fromheretofromhere")
    Strings.from("from")("") shouldBe None
    Strings.from("")("asd") shouldBe Some("asd")
    Strings.from("")("asd") shouldBe Some("asd")
  }

  "The Strings.removeLast" should "remove the last few characters from a string" in {
    Strings.removeLast(0)("asdasd") shouldBe "asdasd"
    Strings.removeLast(2)("asdasd") shouldBe "asda"
  }

  "The Strings.cleanString" should "only allow A-Za-z _- in the name" in {
    Strings.cleanString("") shouldBe ""
    Strings.cleanString("abcABC 123_-") shouldBe "abcABC 123_-"
    Strings.cleanString("abc$£*&^\"\'.ABC 123_-") shouldBe "abcABC 123_-"
  }

  "The Strings.trimChar" should "remove any instances of the trimChar from the start or end of the string" in {
    Strings.trimChar('/')("abc") shouldBe "abc"
    Strings.trimChar('/')("///abc///") shouldBe "abc"
    Strings.trimChar('/')("///../..///") shouldBe "../.."
    Strings.trimChar('/')("../..") shouldBe "../.."
  }

  "The Strings.uri" should "concatenate parts " in {
    Strings.uri("a", "b", "c") shouldBe "a/b/c"
    Strings.uri("/a/", "/b/", "/c/") shouldBe "a/b/c"
    Strings.uri("/a/b/", "/c/") shouldBe "a/b/c"
    Strings.uri("../..", "./c/") shouldBe "../.././c"
  }

  "The Strings.splitLines" should "split whether the newline is #10,#13 or whatever" in {
    Strings.splitLines("") shouldBe Seq()
    Strings.splitLines("abc") shouldBe Seq("abc")
    Strings.splitLines("abc\ndef") shouldBe Seq("abc", "def")
    Strings.splitLines("abc\n\r\fdef") shouldBe Seq("abc", "def")
  }
}
