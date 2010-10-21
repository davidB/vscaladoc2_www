package net_alchim31_vscaladoc2_www

import java.net.URI
import net.liftweb.util.Helpers
import net.liftweb.common.{ Box, Full, Empty, Failure }
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.JsonAST._
import net_alchim31_utils.{ FileSystemHelper, ClasspathHelper }
import java.io.InputStreamReader
import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }
//import org.scalacheck.Gen
import net_alchim31_vscaladoc2_www.info._

@RunWith(classOf[JUnitSuiteRunner])
class JsonSpecTest extends Specification with JUnit /*with ScalaCheck*/ {
  //  "Encoder for 0" should {
  //    //val encoder = encodeFixed(0)
  //
  //    " encode to List[Byte]<xx>" >> {
  //      //encoder(Nil) must_== List[Byte](0)
  //    	1 must_== 0
  //    }
  //  }

  "Json reader" should {
    "read allow " in {
      val uoaHelper = new UoaHelper()
      val rdp : RawDataProvider = new TestProvider(uoaHelper)
      val rdti : RawDataToInfo = new RawDataToInfo(rdp, uoaHelper)
      uoaHelper("vscaladoc_demoprj/0.1-SNAPSHOT").map(x => rdp.find(x)) must_!= Empty
      uoaHelper("vscaladoc_demoprj/0.1-SNAPSHOT/_root_").map(x => rdp.find(x)) must_!= Empty
      for (uoa <- uoaHelper("vscaladoc_demoprj/0.1-SNAPSHOT/_root_")) {
        val l = rdti.findAllTypes(uoa.asInstanceOf[Uoa4Package])
        l.foreach { println }
        l must_!= Nil
      }
    }
    "deny " in {
    }
  }
}


class TestProvider(val uoaHelper: UoaHelper) extends RawDataProvider {

  private val _fsh = new FileSystemHelper()
  private val _cph = new ClasspathHelper()

  def find(uoa: info.Uoa): Box[JValue] = {
    val refPath = uoaHelper.toRefPath(uoa)
    Box.option2Box(_data.get(refPath)).map { x => parse(x) } orElse {
      for (is <- _cph.findCPResourceAsStream("/apidoc/" + refPath + ".json")) yield {
        _fsh.using(new InputStreamReader(is)) { x =>
          parse(x)
        }
      }
    }
  }

  private val _data = Map[String, String](
    "vscaladoc_demoprj/0.1-SNAPSHOT" -> """{
      "title" : "",
      "artifactId" : "vscaladoc_demoprj",
      "version" : "0.1-SNAPSHOT",
      "description" : "",
      "copyright" : "",
      "dependencies" : [ [ "scala-library", "2.8.0" ] ]
    }""")

}