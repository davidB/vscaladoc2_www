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

//import scala.Nothing

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
      val rdp : RawDataProvider = new MyDataProvider(uoaHelper)
      val rdti : RawDataToInfo = new RawDataToInfo(rdp, uoaHelper)
      uoaHelper("vscaladoc_demoprj/0.1-SNAPSHOT").map(x => rdp.find(x)) must_!= Empty
      uoaHelper("vscaladoc_demoprj/0.1-SNAPSHOT/_root_").map(x => rdp.find(x)) must_!= Empty
      for (uoa <- uoaHelper("vscaladoc_demoprj/0.1-SNAPSHOT/_root_")) {
        val l = rdti.findAllTypes(uoa.asInstanceOf[Uoa4Package])
        l.foreach { println }
        l must_!= Nil
      }
    }
    "read Json for Type" in {
      import net.liftweb.json.JsonParser.parse
      implicit val formats = net.liftweb.json.DefaultFormats
      val str0 = """
{
  "uoa" : "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC",
  "e" : [ {
    "name" : "DemoC",
    "qualifiedName" : "itest.demo1.DemoC",
    "definitionName" : "itest.demo1.DemoC",
    "description" : "<p>Class use to test override, inheritance,... (with object DemoC and child class DemoC2)\n</p>",
    "docTags" : [ ],
    "flags" : "Public()",
    "inheritedFrom" : [ ],
    "visibility" : [ ],
    "resultType" : [ [ "DemoC", "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC" ], [ "[T]" ] ],
    "sourceStartPoint" : [ "/home/dwayne/work/oss/vscaladoc2_demoprj/src/main/scala/itest/demo1/DemoC.scala", 16 ],
    "subClassesK" : [ "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC", "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC2" ],
    "members" : [ "scala-library/2.8.0/scala/Any/1.DemoC$hash$bang$eq", "scala-library/2.8.0/scala/Any/1.DemoC$hash$eq$eq", "scala-library/2.8.0/scala/Any/1.DemoC$hashasInstanceOf", "scala-library/2.8.0/scala/Any/1.DemoC$hashisInstanceOf
", "scala-library/2.8.0/scala/AnyRef/emoC$hash$asInstanceOf", "scala-library/2.8.0/scala/AnyRef/emoC$hash$bang$eq", "scala-library/2.8.0/scala/AnyRef/emoC$hash$eq$eq", "scala-library/2.8.0/scala/AnyRef/emoC$hash$hash$hash", "scala-librar
y/2.8.0/scala/AnyRef/emoC$hash$isInstanceOf", "scala-library/2.8.0/scala/AnyRef/emoC$hashclone", "scala-library/2.8.0/scala/AnyRef/emoC$hasheq", "scala-library/2.8.0/scala/AnyRef/emoC$hashequals", "scala-library/2.8.0/scala/AnyRef/emoC$h
ashfinalize", "scala-library/2.8.0/scala/AnyRef/emoC$hashgetClass", "scala-library/2.8.0/scala/AnyRef/emoC$hashhashCode", "scala-library/2.8.0/scala/AnyRef/emoC$hashne", "scala-library/2.8.0/scala/AnyRef/emoC$hashnotify", "scala-library/
2.8.0/scala/AnyRef/emoC$hashnotifyAll", "scala-library/2.8.0/scala/AnyRef/emoC$hashsynchronized", "scala-library/2.8.0/scala/AnyRef/emoC$hashtoString", "scala-library/2.8.0/scala/AnyRef/emoC$hashwait", "vscaladoc_demoprj/0.1-SNAPSHOT/ite
st.demo1/DemoC/ctorArg2", "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC/doStuff", "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC/methodDeprecatedByAnnot", "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC/methodDeprecatedByDoc", "
vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC/this" ],
    "templates" : [ ],
    "methods" : [ "scala-library/2.8.0/scala/Any/1.DemoC$hash$bang$eq", "scala-library/2.8.0/scala/Any/1.DemoC$hash$eq$eq", "scala-library/2.8.0/scala/Any/1.DemoC$hashasInstanceOf", "scala-library/2.8.0/scala/Any/1.DemoC$hashisInstanceOf
", "scala-library/2.8.0/scala/AnyRef/emoC$hash$asInstanceOf", "scala-library/2.8.0/scala/AnyRef/emoC$hash$bang$eq", "scala-library/2.8.0/scala/AnyRef/emoC$hash$eq$eq", "scala-library/2.8.0/scala/AnyRef/emoC$hash$hash$hash", "scala-librar
y/2.8.0/scala/AnyRef/emoC$hash$isInstanceOf", "scala-library/2.8.0/scala/AnyRef/emoC$hashclone", "scala-library/2.8.0/scala/AnyRef/emoC$hasheq", "scala-library/2.8.0/scala/AnyRef/emoC$hashequals", "scala-library/2.8.0/scala/AnyRef/emoC$h
ashfinalize", "scala-library/2.8.0/scala/AnyRef/emoC$hashgetClass", "scala-library/2.8.0/scala/AnyRef/emoC$hashhashCode", "scala-library/2.8.0/scala/AnyRef/emoC$hashne", "scala-library/2.8.0/scala/AnyRef/emoC$hashnotify", "scala-library/
2.8.0/scala/AnyRef/emoC$hashnotifyAll", "scala-library/2.8.0/scala/AnyRef/emoC$hashsynchronized", "scala-library/2.8.0/scala/AnyRef/emoC$hashtoString", "scala-library/2.8.0/scala/AnyRef/emoC$hashwait", "vscaladoc_demoprj/0.1-SNAPSHOT/ite
st.demo1/DemoC/doStuff", "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC/methodDeprecatedByAnnot", "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC/methodDeprecatedByDoc" ],
    "values" : [ "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC/ctorArg2" ],
    "abstractTypes" : [ ],
    "aliasTypes" : [ ],
    "companion" : "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC",
    "parentType" : [ [ "AnyRef" ] ],
    "linearization" : [ "scala-library/2.8.0/scala/AnyRef", "scala-library/2.8.0/scala/Any" ],
    "typeParams" : [ [ "[" ], [ "T" ], [ "]" ] ],
    "isCaseClass" : false,
    "constructors" : [ {
      "name" : "this",
      "qualifiedName" : "itest.demo1.DemoC#this",
      "definitionName" : "itest.demo1.DemoC#this",
      "flags" : "Public()",
      "inheritedFrom" : [ ],
      "visibility" : [ ],
      "resultType" : [ [ "DemoC", "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo1/DemoC" ], [ "[T]" ] ],
      "valueParams" : [ [ "( " ], [ "ctorArg1 : " ], [ "Int", "scala-library/2.8.0/scala/Int" ], [ ", " ], [ "ctorArg2 : " ], [ "String", "jse/1.6.0_21/java.lang/String" ], [ ", " ], [ "ctorArg3 : " ], [ "T" ], [ " )" ] ]
    } ],
    "kind" : "class"

  } ]
}"""

      val jv = parse(str1)
      val tpe = jv.extract[json.Tpe]
//      val ctors = tpeF.e.head.constructors.getOrElse(Nil)
//      val ctor0 = ctors.head
      tpe.name must_== "this"

    	  /*

      val tpeF = jv.extract[json.TpeFile]
      val ctors = tpeF.e.head.constructors.getOrElse(Nil)
      val ctor0 = ctors.head
      ctor0.name must_== "this"
*/
    }

  }
}


class MyDataProvider(val uoaHelper: UoaHelper) extends RawDataProvider {

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