package one.xingyi.cddutilities
import java.util.concurrent.atomic.AtomicInteger

trait IdMaker {
  private val nextId = new AtomicInteger()
  protected def getNextId = nextId.getAndIncrement()

}