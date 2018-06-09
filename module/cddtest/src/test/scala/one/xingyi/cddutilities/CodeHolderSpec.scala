package one.xingyi.cddutilities
/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */


import scala.language.experimental.macros
import scala.language.implicitConversions

class CodeHolderSpec extends CddSpec {

  import CodeHolder._

  val horribleToString =
    """{
      |SerialVersionUID(value = 0) final <synthetic> class $anonfun extends scala.runtime.AbstractPartialFunction[String,String] with Serializable {
      | def <init>(): <$anon: String => String> = {
      |   $anonfun.super.<init>();
      |   ()
      | };
      | final override def applyOrElse[A1 <: String, B1 >: String](x2: A1, default: A1 => B1): B1 = ((x2.asInstanceOf[String]: String): String @unchecked) match {
      |   case (x @ (_: String)) => x.toString()
      |   case (defaultCase$ @ _) => default.apply(x2)
      | };
      | final def isDefinedAt(x2: String): Boolean = ((x2.asInstanceOf[String]: String): String @unchecked) match {
      |   case (x @ (_: String)) => true
      |   case (defaultCase$ @ _) => false
      | }
      |;
      |ew $anonfun()
      |PartialFunction[String,String]])""".stripMargin

  "A code holder" should "have a decent toString for simple functions" in {
    val c: CodeHolder[String => String] = (x: String) => x.toString
    c.description shouldBe "((x: String) => x.toString())"
  }

  it should "turn horrible toStrings of partial functions into nice ones" in {
    CodeHolder.partialFnToStringToNiceToString(horribleToString) shouldBe Some("{case (x @ (_: String)) => x.toString()}")
  }

  it should "have a not totally horrible prettyDescription for partial functions" in {
    CodeHolder.fnToHolder[String => String] { case x => x.toString }.prettyDescription shouldBe "case (x @ _) => x.toString()"
    CodeHolder.fnToHolder[Int => String] { case x: Int => x.toString }.prettyDescription shouldBe "case (x @ (_: Int)) => x.toString()"
    CodeHolder.fnToHolder[PartialFunction[(Int, Int), String]] { case (l, r) => l + "_" + r }.prettyDescription shouldBe "{case (_1: Int, _2: Int)(Int, Int)((l @ _), (r @ _)) => l.+(\"_\").+(r)}"
  }

}
