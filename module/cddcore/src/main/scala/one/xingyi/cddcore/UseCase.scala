package one.xingyi.cddcore
import one.xingyi.cddscenario._
import one.xingyi.cddutilities.DefinedInSourceCodeAt

case class RawSituation2[P, R](data: ScenarioBuilderData[P, R])(implicit a: ScenarioAggregator[P, R]) {
  def produces(r: R): Produces[P, R] = {
    require(data.assertions.size == 0)
    Produces[P, R](data.copy(result = Some(r), assertions = List()))
  }
}

object UseCase1 {
  implicit def hasScenarios: HasScenarios[UseCase1] = new HasScenarios[UseCase1] {
    override def allScenarios[P, R](t: UseCase1[P, R]): List[Scenario[P, R]] = t.allScenarios
  }
}
class UseCase1[P, R](title: String) extends IdMaker {
  protected implicit val aggregator = new RememberingScenarioAggregator[P, R]
  protected def scenario(p: P) = RawSituation2(ScenarioBuilderData[P, R](getNextId, p, title = Some(title), isDefinedAt = DefinedInSourceCodeAt.definedInSourceCodeAt(2)))
  def allScenarios = aggregator.scenarios
  def or(useCase1: UseCase1[P, R]) = new CompositeUseCase[P, R](List(this, useCase1), EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), None))
}

class UseCase2[P1, P2, R](title: String) extends UseCase1[(P1, P2), R](title) {
  protected def scenario(p1: P1, p2: P2) = RawSituation2(ScenarioBuilderData[(P1,P2), R](getNextId, (p1,p2), title = Some(title), isDefinedAt = DefinedInSourceCodeAt.definedInSourceCodeAt(2)))
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

