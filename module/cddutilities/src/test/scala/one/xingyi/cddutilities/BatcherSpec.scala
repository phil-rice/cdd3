package one.xingyi.cddutilities
import one.xingyi.cddutilities.jdbc.{Batcher, BatcherConfig}

class BatcherSpec extends CddSpec {

  behavior of "Batcher"

  it should "batch things up into 'batchsize' and call flush every batchsize" in {
    var batchList = List[Int]()
    var resultList = List[List[Int]]()

    val batcher = new Batcher[Int](BatcherConfig(3, l => batchList = batchList :+ l, { () => resultList = resultList :+ batchList; batchList = List() }))
    List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).foreach(batcher)
    resultList shouldBe List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    batcher.close
    resultList shouldBe List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), List(10))
  }

}
