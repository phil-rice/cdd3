package one.xingyi.cddutilities

import one.xingyi.cddutilities.AnyLanguage._

import scala.collection.concurrent.TrieMap


trait TestFramework[J] extends (CddTest => J)

sealed trait CddTest {
  def fold[D](map: TrieMap[D, CddTest], fn: CddTest => D)
}
case class ScenarioTest(name: String, block: () => Unit) extends CddTest {
  def fold[D](map: TrieMap[D, CddTest], fn: CddTest => D) = fn(this) sideeffect (d => map.getOrElse(d, this))
}
case class NestedTest(name: String, tests: Seq[CddTest]) extends CddTest {
  def fold[D](map: TrieMap[D, CddTest], fn: CddTest => D): Unit = {
    fn(this) sideeffect (d => map.getOrElse(d, this)) sideeffect (_ => tests.foreach(_.fold(map, fn)))
  }
}
