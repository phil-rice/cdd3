package one.xingyi.cddcore
import one.xingyi.cddutilities.DefinedInSourceCodeAt

trait EngineComponent[P, R] {
  def data: EngineComponentData
}

case class EngineComponentData(definedInSourceCodeAt: DefinedInSourceCodeAt, title: Option[String], comment: Option[String] = None, whatsWrongWithMe: List[Exception] = List(), references: List[Reference] = List())


