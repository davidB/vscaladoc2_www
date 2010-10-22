package net_alchim31_vscaladoc2_www

import scala.collection.mutable.ListBuffer
import net.liftweb.util.Helpers
import java.io.FileReader
import java.net.URI
import net.liftweb.json.JsonParser
import net_alchim31_vscaladoc2_www.model.RemoteApiInfo
import net_alchim31_vscaladoc2_www.info._
import net.liftweb.common.{ Box, Full, Empty, Failure }
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.JsonAST._

trait RawDataProvider {
  type UOA = String
  def find(uoa: Uoa): Box[JValue]
}

class RawDataToInfo(val rdp: RawDataProvider, val uoaHelper: UoaHelper) {
  implicit val formats = {
    //    val hints = new ShortTypeHints(classOf[StringWithRef] :: Nil) {
    //      override def serialize: PartialFunction[Any, JObject] = {
    //        case x : StringWithRef => x.uoa match {
    //          case None => JArray(JString(s))
    //        }
    //      }
    //
    //      override def deserialize: PartialFunction[(String, JObject), Any] = {
    //        case ("DateTime", JObject(JField("t", JInt(t)) :: Nil)) => new DateTime(t.longValue)
    //      }
    //    }
    net.liftweb.json.DefaultFormats // Brings in default date formats etc.
  }

  def toArtifactInfo(uoa: Uoa4Artifact): Box[ArtifactInfo] = {
    for (jv <- rdp.find(uoa)) yield {
      new ArtifactInfo() {
        override val artifactId = (jv \ "artifactId").extract[String]
        override val version = (jv \ "version").extract[String]
        override val description = (jv \ "description").extract[String]
      }
    }
  }

  //  protected def toTypeInfo(uoa: Uoa4Type, jo : JObject): Box[TypeInfo] = Helpers.tryo{
  //	new TypeInfo() {
  //	    val simpleName: String = (jo \ "name").extract[String]
  //	    val signature: String = "signature"
  //	    val description: HtmlString = (jo \ "description").extract[String]
  //	    val docTags: Seq[DocTag] = Nil
  //	    val source: Option[URI] = None //sourceStartPoint(0)#(1)
  //	    val uoa: Uoa4Type = uoa
  //	    val kind: String = (jo \ "kind").extract[String]
  //	    def isInherited(m: FieldextInfo) = m.uoa.uoaType == uoa
  //	    val constructors: List[FieldextInfo] = Nil
  //	    val fields: List[FieldextInfo] = Nil
  //	    val methods: List[FieldextInfo] = Nil
  //	  }
  //  }

  def toTypeInfo(uoa: Uoa4Type): List[Box[TypeInfo]] = {
    rdp.find(uoa) match {
      case x: Failure => List(x)
      case Empty => Nil
      case Full(jv) => {
        val tpeFile = jv.extract[json.TpeFile]
        for (tj <- tpeFile.e) yield { Helpers.tryo { new TypeInfo4Json(uoa, tj, uoaHelper) } }
      }

    }
  }

  def findAllTypes(uoa: Uoa4Package): List[Box[Uoa4Type]] = {
    rdp.find(uoa) match {
      case x: Failure => List(x)
      case Empty => Nil
      case Full(jv) => {
        val pkgFile = jv.extract[json.PkgFile]
        //    		for (pkg <- ) yield { Helpers.tryo { new TypeInfo4Json(uoa, tj) } }
        //    		val list = for (JField("members", JArray(list)) <- (jv \ "e" \ "members")) yield list
        //println(list)
        pkgFile.e.flatMap(_.members).distinct.flatMap { refPath =>
          uoaHelper(refPath) match {
            case Full(uoa) =>
              uoa match {
                case u: Uoa4Package => findAllTypes(u)
                case u: Uoa4Type => List(Full(u))
                case x => println("found :" + x); Nil //ignore
              }
            case x: Failure => List(x)
            case Empty => List(Empty)
          }
        }
      }
    }
  }
}



object json {

  type RawSplitStringWithRef = List[List[String]]

  case class PkgFile(uoa: String, e: List[Pkg])
  case class Pkg(
    name: String,
    qualifiedName: String,
    description: Option[String],
    members: List[String],
    packages: List[String])
  case class TpeFile(uoa: String, e: List[Tpe])
  case class Tpe(
    name: String,
    qualifiedName: String,
    description: Option[String],
    resultType: List[List[String]], // [ "DemoB", "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo2/DemoB" ] ],
    //    sourceStartPoint : List[AnyRef], //[ "/home/dwayne/work/oss/vscaladoc2_demoprj/src/main/scala/itest/demo2/DemoB.scala", 6 ],
    methods: List[String], //[ "scala-library/2.8.0/scala/AnyRef/emoB$hash$asInstanceOf", "scala-library/2.8.0/scala/AnyRef/emoB$hash$isInstanceOf", "scala-library/2.8.0/scala/AnyRef/emoB$hashsynchronized", "scala-library/2.8.0/scala/AnyRef/emoB$hashne", "scala-library/2.8.0/scala/AnyRef/emoB$hasheq", "scala-library/2.8.0/scala/AnyRef/emoB$hash$bang$eq", "scala-library/2.8.0/scala/AnyRef/emoB$hash$eq$eq", "scala-library/2.8.0/scala/AnyRef/emoB$hash$hash$hash", "scala-library/2.8.0/scala/AnyRef/emoB$hashfinalize", "scala-library/2.8.0/scala/AnyRef/emoB$hashwait", "scala-library/2.8.0/scala/AnyRef/emoB$hashwait", "scala-library/2.8.0/scala/AnyRef/emoB$hashwait", "scala-library/2.8.0/scala/AnyRef/emoB$hashnotifyAll", "scala-library/2.8.0/scala/AnyRef/emoB$hashnotify", "scala-library/2.8.0/scala/AnyRef/emoB$hashtoString", "scala-library/2.8.0/scala/AnyRef/emoB$hashclone", "scala-library/2.8.0/scala/AnyRef/emoB$hashequals", "scala-library/2.8.0/scala/AnyRef/emoB$hashhashCode", "scala-library/2.8.0/scala/AnyRef/emoB$hashgetClass", "scala-library/2.8.0/scala/Any/2.DemoB$hashasInstanceOf", "scala-library/2.8.0/scala/Any/2.DemoB$hashisInstanceOf", "scala-library/2.8.0/scala/Any/2.DemoB$hash$bang$eq", "scala-library/2.8.0/scala/Any/2.DemoB$hash$eq$eq" ],
    values: List[String],
    //    "abstractTypes" : [ ],
    //    "aliasTypes" : [ ],
    parentType: RawSplitStringWithRef,
    typeParams: RawSplitStringWithRef,
    //    "linearization" : [ "scala-library/2.8.0/scala/AnyRef", "scala-library/2.8.0/scala/Any" ],
    kind: String)
}

class TypeInfo4Json(val uoa: Uoa4Type, val src: json.Tpe, uoaHelper : UoaHelper) extends TypeInfo {
  def toBoxUoa(v : Option[String]) : Box[Uoa4Type] = v match {
	  case None => Empty
	  case Some(v) => uoaHelper(v).flatMap(x => Helpers.tryo{ x.asInstanceOf[Uoa4Type] })
  }
  def toListSWTR(s: List[List[String]]): List[StringWithTypeRef] = s.map(x => StringWithTypeRef(x.head, toBoxUoa(x.tail.headOption)))

  def isCaseClass: Boolean = src.parentType.exists(x => x.head == "Product")
  def simpleName: String = src.name
  def description: HtmlString = src.description.getOrElse("")
  def docTags: Seq[DocTag] = Nil
  def source: Option[URI] = None //for (file <- src.sourceStartPoint.headOption ; line <- src.sourceStartPoint.tail.headOption) yield {new URI("src://" + file + "#" + line) }
  def kind: String = src.kind
  def isInherited(m: FieldextInfo) = m.uoa.uoaType == uoa
  def signature : List[StringWithTypeRef] = {
    val b = new ListBuffer[StringWithTypeRef]()
    if (isCaseClass) {
      b += StringWithTypeRef("case ")
    }
    b += StringWithTypeRef(kind + ' ')
    b += StringWithTypeRef(simpleName)
    if (!src.typeParams.isEmpty) {
      b ++= toListSWTR(src.typeParams)
    }
    if (!src.parentType.isEmpty) {
      b += StringWithTypeRef(" extends ")
      b ++= toListSWTR(src.parentType)
    }
    b.toList
  }
  def constructors: List[FieldextInfo] = Nil
  def fields: List[FieldextInfo] = Nil
  def methods: List[FieldextInfo] = Nil
}
//TODO cache result of remote request
//TODO download archive, store in DB, unarchive in local FS cache
class BasicRawDataProvider() extends RawDataProvider {
  import dispatch._
  import Http._

  private val _http = new Http

  def find(uoa: Uoa): Box[JValue] = {
    for (url <- toUrl(uoa)) yield {
      println("url : " + url)
      url.startsWith("file:/") match {
        case true => JsonParser.parse(new FileReader(new URI(url).getPath))
        case false => _http(new Request(url) >- { s => JsonParser.parse(s) })
      }

    }
  }

  def toUrl(uoa: Uoa): Box[String] = uoa match {
    case Uoa4Artifact(artifactId, version) => {
      for (
        rai <- RemoteApiInfo.findApiOf(artifactId, version);
        rpath <- rai.provider.rurlPathOf()
      ) yield rai.baseUrl.toExternalForm + rpath
    }
    case Uoa4Package(packageName, Uoa4Artifact(artifactId, version)) => {
      for (
        rai <- RemoteApiInfo.findApiOf(artifactId, version);
        rpath <- rai.provider.rurlPathOf(packageName)
      ) yield rai.baseUrl.toExternalForm + rpath
    }
    case Uoa4Type(typeName, Uoa4Package(packageName, Uoa4Artifact(artifactId, version))) => {
      for (
        rai <- RemoteApiInfo.findApiOf(artifactId, version);
        rpath <- rai.provider.rurlPathOf(packageName, typeName)
      ) yield rai.baseUrl.toExternalForm + rpath
    }
    case Uoa4Fieldext(fieldextName, Uoa4Type(typeName, Uoa4Package(packageName, Uoa4Artifact(artifactId, version)))) => {
      for (
        rai <- RemoteApiInfo.findApiOf(artifactId, version);
        rpath <- rai.provider.rurlPathOf(packageName, typeName, fieldextName)
      ) yield rai.baseUrl.toExternalForm + rpath
    }
  }
}