package one.xingyi.cddutilities

object MapsLanguage extends MapsLanguage
trait MapsLanguage {
  implicit class MapOfListsPimper[K, V](map: Map[K, List[V]]) {
    def addToList(kv: (K, V)): Map[K, List[V]] = kv match {case (k, v) => map.get(k).fold(map + (k -> List[V](v)))(list => map + (k -> (list :+ v)))}
    def items(id: K): List[V] = map.getOrElse(id, Nil)
  }
}
