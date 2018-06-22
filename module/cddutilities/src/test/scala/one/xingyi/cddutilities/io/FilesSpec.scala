package one.xingyi.cddutilities.io

import java.io.File

import one.xingyi.cddutilities.CddSpec

import scala.io.Source
import scala.util.Random

class FilesSpec extends CddSpec {

  behavior of "Files"

  it should "print to a file even if that doesn't exist" in {
    val random = new Random(System.currentTimeMillis())
    val someFile = File.createTempFile("testFiles", random.nextString(20))
    someFile.delete()
    val dir = someFile.getParentFile
    val file = new File(new File(dir, "someChild"), "someOtherChild")
    someFile.exists() shouldBe false
    Files.printToFile(file)(_.print("some value"))
    Source.fromFile(file).mkString shouldBe "some value"

  }
}
