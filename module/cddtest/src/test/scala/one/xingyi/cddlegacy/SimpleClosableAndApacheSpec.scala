package one.xingyi.cddlegacy
import one.xingyi.cddutilities.functions.SimpleClosable
import one.xingyi.cddutilities.jdbc.{AbstractFastOrmSpec, DatabaseSourceFixture}
import org.apache.commons.dbcp2.BasicDataSource

trait Apache extends DatabaseSourceFixture[BasicDataSource] {
  override def makeDataSource(): BasicDataSource = {
    val ds = new BasicDataSource
    ds.setDriverClassName("org.h2.Driver")
    ds.setUrl("jdbc:h2:~/test")
    ds.setUsername("sa")
    ds.setPassword("")
    ds
  }
  override def closeDataSource(ds: BasicDataSource): Unit = ds.close()
}
class LegacyJdbcSpec extends AbstractLegacyJdbcSpec[SimpleClosable, BasicDataSource] with Apache
class FastOrmSpec extends AbstractFastOrmSpec[SimpleClosable, BasicDataSource] with Apache