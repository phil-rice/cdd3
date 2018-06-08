package one.xingyi.cddscenario
import one.xingyi.cddutilities.IdMaker

object UntypedScenarioBuilder extends UntypedScenarioBuilder
trait UntypedScenarioBuilder extends IdMaker {
  def scenario[P](p: P): RawSituation[P] = RawSituation(p, ScenarioBuilderData(getNextId, p))
  def scenario[P1, P2](p1: P1, p2: P2): RawSituation[(P1, P2)] = RawSituation((p1, p2), ScenarioBuilderData(getNextId, (p1, p2)))
  def scenario[P1, P2, P3](p1: P1, p2: P2, p3: P3): RawSituation[(P1, P2, P3)] = RawSituation((p1, p2, p3), ScenarioBuilderData(getNextId, (p1, p2, p3)))
  def scenario[P1, P2, P3, P4](p1: P1, p2: P2, p3: P3, p4: P4): RawSituation[(P1, P2, P3, P4)] = RawSituation((p1, p2, p3, p4), ScenarioBuilderData(getNextId, (p1, p2, p3, p4)))
  def scenario[P1, P2, P3, P4, P5](p1: P1, p2: P2, p3: P3, p4: P4, p5: P5): RawSituation[(P1, P2, P3, P4, P5)] = RawSituation((p1, p2, p3, p4, p5), ScenarioBuilderData(getNextId, (p1, p2, p3, p4, p5)))
}
