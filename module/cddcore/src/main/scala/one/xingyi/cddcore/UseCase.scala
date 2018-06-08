package one.xingyi.cddcore
import one.xingyi.cddscenario
import one.xingyi.cddscenario._
import one.xingyi.cddutilities.{DefinedInSourceCodeAt, IdMaker}


object UseCase1 {
  implicit def hasScenarios: HasScenarios[UseCase1] = new HasScenarios[UseCase1] {
    override def allScenarios[P, R](t: UseCase1[P, R]): List[Scenario[P, R]] = t.allScenarios
  }
}
class UseCase1[P, R](title: String) extends IdMaker {
  protected implicit val aggregator = new RememberingScenarioAggregator[P, R]
  protected def scenario(p: P) = RawSituation[P](p, ScenarioBuilderData[P, Nothing](getNextId, p, title = Some(title), isDefinedAt = DefinedInSourceCodeAt.definedInSourceCodeAt(2)))
  def allScenarios = aggregator.scenarios
  def or(useCase1: UseCase1[P, R]) = new CompositeUseCase[P, R](List(this, useCase1), EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), None))
}

class UseCase2[P1, P2, R](title: String) extends UseCase1[(P1, P2), R](title) {
  protected def scenario(p1: P1, p2: P2): RawSituation2[P1, P2] = {
    val data = ScenarioBuilderData[(P1, P2), Nothing](getNextId, (p1, p2), title = Some(title), isDefinedAt = DefinedInSourceCodeAt.definedInSourceCodeAt(2))
    RawSituation2(p1, p2, data)
  }
}

object CompositeUseCase {
  implicit def hasScenarios = new HasScenarios[CompositeUseCase] {
    override def allScenarios[P, R](t: CompositeUseCase[P, R]): List[Scenario[P, R]] = t.allScenarios
  }
}
class CompositeUseCase[P, R](useCases: List[UseCase1[P, R]], engineComponentData: EngineComponentData) {
  val allScenarios: List[Scenario[P, R]] = useCases.flatMap(_.allScenarios)
  def or(useCase1: UseCase1[P, R]) = new CompositeUseCase[P, R](useCases :+ useCase1, EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), None))
}

