package one.xingyi.cddscenario

import one.xingyi.cddutilities.DefinedInSourceCodeAt

trait HasEngineComponentData[T] extends (T => EngineComponentData)
case class EngineComponentData(definedInSourceCodeAt: DefinedInSourceCodeAt, title: Option[String], comment: Option[String] = None, whatsWrongWithMe: List[Exception] = List(), references: List[Reference] = List())


