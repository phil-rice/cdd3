package one.xingyi.cddutilities

class AddedFinderNotActuallAnException extends Exception

object DefinedInSourceCodeAt {
  protected val defaultStackTraceOffset = 5

  def definedInSourceCodeAt(stackTraceOffset: Int = defaultStackTraceOffset) = {
//    println(new AddedFinderNotActuallAnException().getStackTrace().mkString("\n"))
    new DefinedInSourceCodeAt(new AddedFinderNotActuallAnException().getStackTrace()(stackTraceOffset))
  }
}

class DefinedInSourceCodeAt(val st: StackTraceElement) {
  override lazy val toString = {
    val s = st.toString
    val i = s.lastIndexOf("(")
    s.substring(i)
  }

  override def equals(other: Any) = other match {
    case d: DefinedInSourceCodeAt => d.toString == toString && d.getClass == getClass
    case _ => false
  }

  override def hashCode = toString.hashCode
}
