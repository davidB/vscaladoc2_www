package net_alchim31_vscaladoc2_www

import net_alchim31_utils.FileSystemHelper

import java.net.MalformedURLException
import java.io.File
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
        override val groupId = (jv \ "groupId").extract[String]
        override val artifactId = (jv \ "artifactId").extract[String]
        override val version = (jv \ "version").extract[String]
        override val description = (jv \ "description").extract[String]
        override val logo = (jv \ "logo").extract[String]
        override val license = (jv \ "license").extract[String]
      }
    }
  }


  def toTypeInfo(uoa: Uoa4Type): List[Box[TypeInfo]] = {
    rdp.find(uoa) match {
      case x: Failure => List(x)
      case Empty => Nil
      case Full(jv) => {
        val tpeFile = jv.extract[json.TpeFile]
        for (tj <- tpeFile.e) yield { Helpers.tryo { new TypeInfo4Json(uoa, tj, this) } }
      }
    }
  }

  def toFieldextInfo(uoa: Uoa4Fieldext): List[Box[FieldextInfo]] = {
    rdp.find(uoa) match {
      case x: Failure => List(x)
      case Empty => Nil
      case Full(jv) => {
        val file = jv.extract[json.FieldextFile]
        for (entry <- file.e) yield { Helpers.tryo { new FieldextInfo4Json(uoa, entry, this) } }
      }

    }
  }

  // provide one entry for object and trait/class (remove $object entry)
  // TODO cache ?
  def findAllTypes(uoa: Uoa4Package): List[Box[Uoa4Type]] = {
    rdp.find(uoa) match {
      case x: Failure => List(x)
      case Empty => Nil
      case Full(jv) => {
        val pkgFile = jv.extract[json.PkgFile]
        //.map(x => if (excludeObjectSuffix) removeObjectSuffix(x) else x)
        pkgFile.e.flatMap(_.templates).distinct.flatMap { refPath =>
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

  def toBoxUoa(v: Option[String]): Box[Uoa] = v match {
    case None => Empty
    case Some(v) => uoaHelper(v)
  }
  def toListSWTR(s: List[List[String]]): List[StringWithTypeRef] = s.map(x => StringWithTypeRef(x.head, toBoxUoa(x.tail.headOption)))

  def toListFieldext(s : List[String]) : List[Box[FieldextInfo]] = {
    val b = for(
        refPath <- s;
        uoa <- uoaHelper(refPath) //ignore failure and empty
      ) yield uoa match {
        case uoa : Uoa4Fieldext => toFieldextInfo(uoa)
        case _ => Nil
      }
    b.flatten
  }


}

object json {

  type RawSplitStringWithRef = List[List[String]]

  case class PkgFile(uoa: String, e: List[Pkg])
  case class Pkg(
    name: String,
    qualifiedName: String,
    description: Option[String],
    templates: List[String],
    packages: List[String])
  case class TpeFile(uoa: String, e: List[Tpe])
  case class Tpe(
    name: String,
    qualifiedName: String,
    description: Option[String],
    visibility: RawSplitStringWithRef,
    //resultType: RawSplitStringWithRef, // [ "DemoB", "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo2/DemoB" ] ],
    //    sourceStartPoint : List[AnyRef], //[ "/home/dwayne/work/oss/vscaladoc2_demoprj/src/main/scala/itest/demo2/DemoB.scala", 6 ],
    methods: List[String], //[ "scala-library/2.8.0/scala/AnyRef/emoB$hash$asInstanceOf", "scala-library/2.8.0/scala/AnyRef/emoB$hash$isInstanceOf", "scala-library/2.8.0/scala/AnyRef/emoB$hashsynchronized", "scala-library/2.8.0/scala/AnyRef/emoB$hashne", "scala-library/2.8.0/scala/AnyRef/emoB$hasheq", "scala-library/2.8.0/scala/AnyRef/emoB$hash$bang$eq", "scala-library/2.8.0/scala/AnyRef/emoB$hash$eq$eq", "scala-library/2.8.0/scala/AnyRef/emoB$hash$hash$hash", "scala-library/2.8.0/scala/AnyRef/emoB$hashfinalize", "scala-library/2.8.0/scala/AnyRef/emoB$hashwait", "scala-library/2.8.0/scala/AnyRef/emoB$hashwait", "scala-library/2.8.0/scala/AnyRef/emoB$hashwait", "scala-library/2.8.0/scala/AnyRef/emoB$hashnotifyAll", "scala-library/2.8.0/scala/AnyRef/emoB$hashnotify", "scala-library/2.8.0/scala/AnyRef/emoB$hashtoString", "scala-library/2.8.0/scala/AnyRef/emoB$hashclone", "scala-library/2.8.0/scala/AnyRef/emoB$hashequals", "scala-library/2.8.0/scala/AnyRef/emoB$hashhashCode", "scala-library/2.8.0/scala/AnyRef/emoB$hashgetClass", "scala-library/2.8.0/scala/Any/2.DemoB$hashasInstanceOf", "scala-library/2.8.0/scala/Any/2.DemoB$hashisInstanceOf", "scala-library/2.8.0/scala/Any/2.DemoB$hash$bang$eq", "scala-library/2.8.0/scala/Any/2.DemoB$hash$eq$eq" ],
    values: List[String],
    //    "abstractTypes" : [ ],
    //    "aliasTypes" : [ ],
    parentType: RawSplitStringWithRef,
    typeParams: RawSplitStringWithRef,
    //    "linearization" : [ "scala-library/2.8.0/scala/AnyRef", "scala-library/2.8.0/scala/Any" ],
    constructors : List[Fieldext],
    kind: String)
  case class FieldextFile(uoa: String, e: List[Fieldext])
  case class Fieldext(
    name: String,
    qualifiedName: String,
    description: Option[String],
    visibility: RawSplitStringWithRef,
    resultType: RawSplitStringWithRef,
    valueParams: RawSplitStringWithRef,
    typeParams: RawSplitStringWithRef,
    kind: String)
}

class TypeInfo4Json(val uoa: Uoa4Type, val src: json.Tpe, rdti : RawDataToInfo) extends TypeInfo {

  def isCaseClass: Boolean = src.parentType.exists(x => x.head == "Product")
  def simpleName: String = src.name
  def description: HtmlString = src.description.getOrElse("")
  def docTags: Seq[DocTag] = Nil
  def source: Option[URI] = None //for (file <- src.sourceStartPoint.headOption ; line <- src.sourceStartPoint.tail.headOption) yield {new URI("src://" + file + "#" + line) }
  def kind: String = src.kind
  def isInherited(m: FieldextInfo) = m.uoa.uoaType != uoa
  def signature: List[StringWithTypeRef] = {
    val b = new ListBuffer[StringWithTypeRef]()
    b ++= rdti.toListSWTR(src.visibility)
    if (isCaseClass) {
      b += StringWithTypeRef("case ")
    }
    b += StringWithTypeRef(kind + ' ')
    b += StringWithTypeRef(simpleName)
    if (!src.typeParams.isEmpty) {
      b ++= rdti.toListSWTR(src.typeParams)
    }
    val parents = src.parentType.filter(x => x.head != "AnyRef" && !(isCaseClass && x.head == "Product"))
    if (!parents.isEmpty) {
      b += StringWithTypeRef(" extends ")
      b ++= rdti.toListSWTR(parents)
    }
    b.toList
  }
  def constructors: List[Box[FieldextInfo]] = {
      src.constructors.map(x => Helpers.tryo{ new FieldextInfo4Json(Uoa4Fieldext(simpleName, uoa), x, rdti)})
  }
  def fields: List[Box[FieldextInfo]] = rdti.toListFieldext(src.values)
  def methods: List[Box[FieldextInfo]] = rdti.toListFieldext(src.methods)
}

class FieldextInfo4Json(val uoa: Uoa4Fieldext, val src: json.Fieldext, rdti : RawDataToInfo) extends FieldextInfo {

  def simpleName: String = src.name
  def description: HtmlString = src.description.getOrElse("")
  def docTags: Seq[DocTag] = Nil
  def source: Option[URI] = None //for (file <- src.sourceStartPoint.headOption ; line <- src.sourceStartPoint.tail.headOption) yield {new URI("src://" + file + "#" + line) }
  def kind: String = src.kind
  def signature: List[StringWithTypeRef] = {
    val b = new ListBuffer[StringWithTypeRef]()
    b ++= rdti.toListSWTR(src.visibility)
    b += StringWithTypeRef(kind + ' ')
    b += StringWithTypeRef(simpleName)
    if (!src.typeParams.isEmpty) {
      b ++= rdti.toListSWTR(src.typeParams)
    }
    if (!src.valueParams.isEmpty) {
      b ++= rdti.toListSWTR(src.valueParams)
    }
    if (!src.resultType.isEmpty) {
      b += StringWithTypeRef(" : ")
      b ++= rdti.toListSWTR(src.resultType)
    }
    b.toList
  }
}
//TODO cache result of remote request
//TODO download archive, store in DB, unarchive in local FS cache
//TODO Http is not multi-thread, may be used a pool
//TODO simplify
class BasicRawDataProvider(fs : FileSystemHelper, workdir : File) extends RawDataProvider {
  import dispatch._
  import Http._

  private val localApisDir = new File(workdir, "apis")

  def find(uoa: Uoa): Box[JValue] = {
    toUrl(uoa).flatMap { uri =>
        Helpers.tryo{
          uri.getScheme match {
            case "file" => JsonParser.parse(new FileReader(uri.getPath))
            case "local" => JsonParser.parse(new FileReader(toLocalFile(uri.getPath)))
            case "http" => JsonParser.parse(requestFromHttp(uoa, uri))
            case x => throw new MalformedURLException("scheme " + x + "is nor supported as source for api (" + uri +")" )
          }
        }
    }
  }

  /**
   * search from local base (use as cache), if missed then do remote request and store result in cache.
   * 
   * @param uoa
   * @param uri
   * @return
   */
  private def requestFromHttp(uoa : Uoa, uri : URI) : String = {
    toLocalFile(uoa).map { f =>
      if (f.exists) {
        fs.toString(f)
      } else {
        new Http()(new Request(uri.toString) >- { s =>
          fs.toFile(f, s)
          s 
        })
      }
    } openOr {
        new Http()(new Request(uri.toString) >- { s =>
          s 
        })
    }
  }


  private def toUrl(uoa: Uoa): Box[URI] = {
      import net_alchim31_vscaladoc2_www.model.VScaladoc2

      def vscaladoc2Rai(artifactId : String, version : String) = {
          RemoteApiInfo.findApiOf(artifactId, version).flatMap { rai =>
            rai.provider match {
                case VScaladoc2 => Full(rai)
                case _ => Failure("data could only be merge from VScaladoc2 source : " + uoa + " is in format " + rai.provider + " located at " + rai.baseUrl)
            }
          }
      }
      uoa match {
        case Uoa4Artifact(artifactId, version) => {
          for (
            rai <- vscaladoc2Rai(artifactId, version);
            rpath <- rai.provider.rurlPathOf()
          ) yield new URI(rai.baseUrl.toString + rpath)
        }
        case Uoa4Package(packageName, Uoa4Artifact(artifactId, version)) => {
          for (
            rai <- vscaladoc2Rai(artifactId, version);
            rpath <- rai.provider.rurlPathOf(packageName)
          ) yield new URI(rai.baseUrl.toString + rpath)
        }
        case Uoa4Type(typeName, Uoa4Package(packageName, Uoa4Artifact(artifactId, version))) => {
          for (
            rai <- vscaladoc2Rai(artifactId, version);
            rpath <- rai.provider.rurlPathOf(packageName, typeName)
          ) yield new URI(rai.baseUrl.toString + rpath)
        }
        case Uoa4Fieldext(fieldextName, Uoa4Type(typeName, Uoa4Package(packageName, Uoa4Artifact(artifactId, version)))) => {
          for (
            rai <- vscaladoc2Rai(artifactId, version);
            rpath <- rai.provider.rurlPathOf(packageName, typeName, fieldextName)
          ) yield new URI(rai.baseUrl.toString + rpath)
        }
      }
  }

  private def toLocalFile(rpath : String) : File = new File(localApisDir, rpath)

  private def toLocalFile(uoa: Uoa): Box[File] = {
      import net_alchim31_vscaladoc2_www.model.VScaladoc2
      val rpath = uoa match {
        case Uoa4Artifact(artifactId, version) => {
          for (
            rpath <- VScaladoc2.rurlPathOf()
          ) yield artifactId + "/" + version + rpath
        }
        case Uoa4Package(packageName, Uoa4Artifact(artifactId, version)) => {
          for (
            rpath <- VScaladoc2.rurlPathOf(packageName)
          ) yield artifactId + "/" + version + rpath
        }
        case Uoa4Type(typeName, Uoa4Package(packageName, Uoa4Artifact(artifactId, version))) => {
          for (
            rpath <- VScaladoc2.rurlPathOf(packageName, typeName)
          ) yield artifactId + "/" + version + rpath
        }
        case Uoa4Fieldext(fieldextName, Uoa4Type(typeName, Uoa4Package(packageName, Uoa4Artifact(artifactId, version)))) => {
          for (
            rpath <- VScaladoc2.rurlPathOf(packageName, typeName, fieldextName)
          ) yield artifactId + "/" + version + rpath
        }
      }
      rpath.map( x => toLocalFile(x))
  }
}