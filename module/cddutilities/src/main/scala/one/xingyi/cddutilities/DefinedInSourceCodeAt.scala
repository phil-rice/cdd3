package one.xingyi.cddutilities


class AddedFinderNotActuallAnException extends Exception

trait IsDefinedInSourceCodeAt[T] extends (T => DefinedInSourceCodeAt)

object DefinedInSourceCodeAt {
  protected val defaultStackTraceOffset = 5

  def definedInSourceCodeAt(stackTraceOffset: Int = defaultStackTraceOffset) =
    new SingleDefinedInSourceCodeAt(new AddedFinderNotActuallAnException().getStackTrace()(stackTraceOffset))

  def toSeq: DefinedInSourceCodeAt => Seq[SingleDefinedInSourceCodeAt] = {case s: SingleDefinedInSourceCodeAt => List(s); case CompositeDefinedInSourceCodeAt(defined) => defined}
  implicit val semiGroupForDefinedAt: SemiGroup[DefinedInSourceCodeAt] = (t1, t2) => CompositeDefinedInSourceCodeAt(toSeq(t1) ++ toSeq(t2))
}

object DefinedInSourceCodeAtLanguage extends DefinedInSourceCodeAtLanguage
trait DefinedInSourceCodeAtLanguage {
  implicit class DefinedInSourceCodeAtOps[T](t: T)(implicit definedAt: IsDefinedInSourceCodeAt[T]) {
    def definedInSourceCodeAt = definedAt(t)
  }
}

trait DefinedInSourceCodeAt

class SingleDefinedInSourceCodeAt(val st: StackTraceElement) extends DefinedInSourceCodeAt {
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

case class CompositeDefinedInSourceCodeAt(defined: Seq[SingleDefinedInSourceCodeAt]) extends DefinedInSourceCodeAt {
  override def toString: String = s"Defined at[${defined.map(_.toString).mkString(",")}]"
}