package one.xingyi.cddscenario

object NoDefaultDefinedException {
  def throwWith[P]: PartialFunction[P, Nothing] = {case p if false => throw new NoDefaultDefinedException(p)}
}
case class NoDefaultDefinedException(p: Any) extends RuntimeException(s"Calling apply using $p without a default defined")

case class UndefinedException(logic: Seq[PartialFunction[_, _]], p: Any) extends RuntimeException(s"Cannot execute because undefined at $p. \nLogic is\n${logic.mkString("\n")}")
