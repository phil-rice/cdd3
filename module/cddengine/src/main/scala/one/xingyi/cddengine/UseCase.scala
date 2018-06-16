package one.xingyi.cddengine
import one.xingyi.cddscenario
import one.xingyi.cddscenario._
import one.xingyi.cddutilities.{DefinedInSourceCodeAt, IdMaker}
import scala.language.higherKinds

trait HasUseCases[T[_, _]] {
  def useCases[P, R](t: T[P, R]): List[UseCase[P, R]]
}

object UseCase {
  implicit def ucHasComponentData[P, R]: HasEngineComponentData[UseCase[P, R]] = { u => u.data }

}
trait UseCase[P, R] {
  def data: EngineComponentData
  def allScenarios: List[Scenario[P, R]]
  def allUseCases: List[UseCase[P, R]]
}
object UseCase1 {
  implicit def hasScenarios: HasScenarios[UseCase1] = new HasScenarios[UseCase1] {
    override def allScenarios[P, R](t: UseCase1[P, R]): List[Scenario[P, R]] = t.allScenarios
  }
  implicit def usHasUseCases: HasUseCases[UseCase1] = new HasUseCases[UseCase1] {
    override def useCases[P, R](t: UseCase1[P, R]): List[UseCase1[P, R]] = List(t)
  }
}
class UseCase1[P, R](val data: EngineComponentData) extends UseCase[P, R] with EngineBuilderLanguage1 {
  def this(title: String) = this(EngineComponentData(definedInSourceCodeAt = DefinedInSourceCodeAt.definedInSourceCodeAt(), title = Some(title)))
  protected implicit val aggregator = new RememberingScenarioAggregator2[P, R]
  def allScenarios = aggregator.scenarios
  def or(useCase1: UseCase1[P, R]) = new CompositeUseCase[P, R](List(this, useCase1), EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), None))
  override def allUseCases: List[UseCase[P, R]] = List()
}

class UseCase2[P1, P2, R](data: EngineComponentData) extends UseCase1[(P1, P2), R](data) with EngineBuilderLanguage2 {
  def this(title: String) = this(EngineComponentData(definedInSourceCodeAt = DefinedInSourceCodeAt.definedInSourceCodeAt(), title = Some(title)))
//  protected def scenario(p1: P1, p2: P2): RawSituation2[P1, P2] = {
//    val data = ScenarioBuilderData[(P1, P2), Nothing](getNextId, (p1, p2), title = None, isDefinedAt = DefinedInSourceCodeAt.definedInSourceCodeAt(2))
//    RawSituation2(p1, p2, data)
//  }
}

object CompositeUseCase {
  implicit def hasScenarios = new HasScenarios[CompositeUseCase] {
    override def allScenarios[P, R](t: CompositeUseCase[P, R]): List[Scenario[P, R]] = t.allScenarios
  }
  implicit def usHasUseCases: HasUseCases[CompositeUseCase] = new HasUseCases[CompositeUseCase] {
    override def useCases[P, R](t: CompositeUseCase[P, R]): List[UseCase1[P, R]] = t.allUseCases
  }
}
class CompositeUseCase[P, R](val allUseCases: List[UseCase1[P, R]], val data: EngineComponentData) extends UseCase[P, R] {
  val allScenarios: List[Scenario[P, R]] = allUseCases.flatMap(_.allScenarios)
  def or(useCase1: UseCase1[P, R]) = new CompositeUseCase[P, R](allUseCases :+ useCase1, EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), None))
}

