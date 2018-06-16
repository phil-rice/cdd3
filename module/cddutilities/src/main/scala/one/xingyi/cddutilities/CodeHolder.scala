/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddutilities

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.blackbox


object CodeHolder {

  def partialFnToStringToNiceToString(s: String) = {
    val firstIndex = s.indexOf("case")
    if (firstIndex == -1)
      None
    else {
      val secondIndex = s.indexOf("case", firstIndex + 1)
      if (secondIndex == -1)
        None
      else
        Some("{" + s.substring(firstIndex, secondIndex).trim + "}")
    }
  }
//  def prettyDescription(description: String) = description match {
  //    case d if description.contains(
  //      "SerialVersionUID(value = 0) final <synthetic> class $anonfun extends scala.runtime.AbstractPartialFunction[") => CodeHolder.partialFnToStringToNiceToString(d).getOrElse(d)
  //    case d if description.startsWith("((") && description.endsWith("})") && description.contains("match") => {
  //      Strings.from("case")(d).map(Strings.removeLast(2)).map(_.trim).getOrElse(d)
  //    }
  //    case d => d
  //  }

}

