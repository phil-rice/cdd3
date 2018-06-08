package one.xingyi.cddscenario
import scala.annotation.implicitNotFound

// yeah I know. This is all about the side effects. I don't know how to make a DSL with nice error messages that doesn't have side effects
@implicitNotFound("""If you are making the scenario using a use case you will have one of these. If you NEED to have no aggregator and know what you are doing, you can import the NullScenarioAggregator""")
trait ScenarioAggregator[P, R] extends (ScenarioBuilderComponent[_, P, R] => Unit)

object NullScenarioAggregator {
  implicit def nullAggregator[P, R]: ScenarioAggregator[P, R] = scenario => {}
}

class RememberingScenarioAggregator[P, R] extends ScenarioAggregator[P, R] {
  private var list = List[ScenarioBuilderComponent[_, P, R]]()
  private val lock = new Object()
  override def apply(comp: ScenarioBuilderComponent[_, P, R]): Unit = lock.synchronized(
    list = list.filterNot(_.data.id == comp.data.id) :+ comp
  )
  def withAggreator[X](fn: ScenarioAggregator[P, R] => X): (X, List[Scenario[P, R]]) = {
    val x = fn(this)
    (x, list.map(_.scenario))
  }
  def scenarios = list.map(_.scenario)
}

