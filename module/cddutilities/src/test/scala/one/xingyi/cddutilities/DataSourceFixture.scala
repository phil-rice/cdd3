package one.xingyi.cddutilities
import javax.sql.DataSource
import org.scalatest.BeforeAndAfterAll

trait DatabaseSourceFixture[DS <: DataSource] extends CddSpec with BeforeAndAfterAll {
  def makeDataSource(): DS
  def closeDataSource(ds: DS)
  val ds: DS = makeDataSource()

  override protected def afterAll(): Unit = {
    closeDataSource(ds)
    super.afterAll()
  }

}