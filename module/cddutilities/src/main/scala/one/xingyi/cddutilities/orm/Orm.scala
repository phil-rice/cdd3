package one.xingyi.cddutilities.orm

import java.sql.ResultSet

import javax.sql.DataSource
import one.xingyi.cddutilities.functions.{ClosableM, Streams}

import scala.language.higherKinds

trait FastReader[T] extends (MainEntity => Stream[T])
object FastReader {
  def apply[T](batchConfig: BatchConfig)(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql) = new FastReaderImpl[T](batchConfig)
}

// We might need to reconsider this interface. At the moment there is a different connection for each operation, but there is something cool about the idea of
// running the operations in a connection and the temp tables vanishing at the end of the connection
trait FastReaderDal extends {
  def execute(ds: DataSource): String => Unit
  def query(ds: DataSource): String => List[List[AnyRef]]
}
object FastReaderDal {
  import one.xingyi.cddutilities.jdbc.Jdbc._
  implicit def defaultFastReaderOps[M[_] : ClosableM]: FastReaderDal = new FastReaderDal {
    def execute(ds: DataSource): String => Unit = { s: String => executeSql(s) apply ds }
    def query(ds: DataSource): String => List[List[AnyRef]] = { s: String => getList(s) { rs: ResultSet => (1 to rs.getMetaData.getColumnCount).toList.map(rs.getObject) } apply ds }
  }
}

case class BatchConfig(dataSource: DataSource, batchSize: Int)

case class BatchDetails(batchSize: Int, index: Int) {
  def offset: Int = batchSize * index
}
class FastReaderImpl[T](batchConfig: BatchConfig)(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql) extends FastReader[T] {
  import batchConfig.dataSource
  import fastReaderOps._

  //TODO Clean this. There is some composition going on here that is interesting.
  def makeBatch(n: Int, main: MainEntity): Option[(List[T], MainEntity)] = {
    OrmStrategies.dropTempTables.map(execute(dataSource)).walk(main)
    OrmStrategies.createTempTables(BatchDetails(batchConfig.batchSize, n)).map(execute(dataSource)).walk(main)
    val list = ormMaker(OrmStrategies.drainTempTables.map(query(dataSource)).walk(main).toMap)
    if (list.isEmpty) None else Some((list, main))
  }
  override def apply(mainEntity: MainEntity): Stream[T] = Streams.unfoldIndexedList(0, mainEntity)(makeBatch)
}



