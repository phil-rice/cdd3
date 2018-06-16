package one.xingyi.cddscenario

import one.xingyi.cddutilities.{DefinedInSourceCodeAt, Lens, SingleDefinedInSourceCodeAt}

trait HasEngineComponentData[T] extends (T => EngineComponentData)

object EngineComponentData {
  def toTitleL: Lens[EngineComponentData, Option[String]] = Lens(_.title, (d, t) => d.copy(title = t))
  def toCommentL: Lens[EngineComponentData, Option[String]] = Lens(_.comment, (d, c) => d.copy(comment = c))
  def toReferencesL: Lens[EngineComponentData, List[Reference]] = Lens(_.references, (d, r) => d.copy(references = r))
}
case class EngineComponentData(definedInSourceCodeAt: SingleDefinedInSourceCodeAt, title: Option[String], comment: Option[String] = None, whatsWrongWithMe: List[Exception] = List(), references: List[Reference] = List())


