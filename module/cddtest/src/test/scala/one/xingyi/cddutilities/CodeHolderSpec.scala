/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddutilities


import one.xingyi.cddutilities.functions.CodeHolder

import scala.language.experimental.macros
import scala.language.implicitConversions

class CodeHolderSpec extends CddSpec {

  import one.xingyi.cddutilities.functions.CodeHolder._

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



  it should "turn horrible toStrings of partial functions into nice ones" in {
    CodeHolder.partialFnToStringToNiceToString(horribleToString) shouldBe Some("{case (x @ (_: String)) => x.toString()}")
  }


}
