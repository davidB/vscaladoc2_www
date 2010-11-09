package net_alchim31_vscaladoc2_www

import net_alchim31_utils.FileSystemHelper

import java.net.MalformedURLException
import java.io.File
import scala.collection.mutable.ListBuffer
import net.liftweb.util.Helpers
import java.io.FileReader
import java.net.URI
import net.liftweb.json.JsonParser
import net_alchim31_vscaladoc2_www.info._
import net.liftweb.common.{ Box, Full, Empty, Failure, Loggable }
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.JsonAST._

trait RawDataProvider {
  type UOA = String
  def find(uoa: Uoa): Box[JValue]
}

trait InfoDataProvider {
  def toArtifactInfo(uoa: Uoa4Artifact): Box[ArtifactInfo]
  def toPackageInfo(uoa: Uoa4Package): List[Box[PackageInfo]]
  def toTypeInfo(uoa: Uoa4Type): List[Box[TypeInfo]]
  def toFieldextInfo(uoa: Uoa4Fieldext): List[Box[FieldextInfo]]
  def findAllTypes(uoa: Uoa4Artifact): List[Box[Uoa4Type]]
}

class ApiService(lazy_idp : () => InfoDataProvider) {
  import net_alchim31_vscaladoc2_www.model.{RemoteApiInfo, ApiProviders, VScaladoc2}

  private lazy val idp = lazy_idp()
  
  def init() {
    if (RemoteApiInfo.find() == Empty){
    val data = List(
        //("vscaladoc2_demoprj", "0.1-SNAPSHOT", new URI("local:/"), ApiProviders.vscaladoc2),
        //("vscaladoc2_demoprj", "0.1-SNAPSHOT", new URI("http://davidb.github.com/vscaladoc2_demoprj/vscaladoc2_demoprj/0.1-SNAPSHOT"), ApiProviders.vscaladoc2),
        ("vscaladoc2_demoprj", "0.1-SNAPSHOT", new URI("http://alchim31.free.fr/apis/vscaladoc2_demoprj/0.1-SNAPSHOT"), ApiProviders.vscaladoc2),
        //("framework_2.8.0", "2.2-M1", new URI("local:/"), ApiProviders.vscaladoc2)
        ("scala-library", "2.8.0", new URI("http://alchim31.free.fr/apis/scala-library/2.8.0"), ApiProviders.vscaladoc2),
        ("scala-library", "2.7.7", new URI("http://www.scala-lang.org/api/2.7.7"), ApiProviders.scaladoc)
    ).foreach { x =>
        val v: RemoteApiInfo = RemoteApiInfo.create
        v.artifactId(x._1)
        v.version(x._2)
        v.url(x._3.toASCIIString)
        v.format(x._4)
        register(v)
      }
    }
  }
  
  def findApiOf(artifactId: String, version: String): Box[RemoteApiInfo] = RemoteApiInfo.findApiOf(artifactId, version)

  def register(v : RemoteApiInfo) : List[Box[ArtifactInfo]]= {
    // TODO check if the remote api is available or not
    v.save
    val bartifact = idp.toArtifactInfo(Uoa4Artifact(v.artifactId.is, v.version.is))
    println("register : " + Uoa4Artifact(v.artifactId.is, v.version.is) + " => "+ bartifact)
    val childrenRegistration : List[Box[ArtifactInfo]] = v.provider match {
      case VScaladoc2 => {
        val children = bartifact.map(_.artifacts).openOr(Nil)
        println("children :" + children + " <- " + bartifact.map(_.artifacts.length))
        children.flatMap { uoa =>
          findApiOf(uoa.artifactId, uoa.version) match {
            case x : Failure => {
              // TODO check if Failure for not registered 
              val v2: RemoteApiInfo = RemoteApiInfo.create
              v2.artifactId(uoa.artifactId)
              v2.version(uoa.version)
              val url = v.url.is match {
                case "local:/" => "local:/"
                case x => x + "/../../" + uoa.artifactId + "/" + uoa.version
              }
              v2.url(url)
              v2.format(v.format)
              println("try to register :" + v2)
              register(v2)
            }
            case _ => Nil //ignore
          }
        }
      }
      case _ => Nil
    }
    bartifact :: childrenRegistration  
  }
}

class InfoDataProvider0(val rdp: RawDataProvider, val uoaHelper: UoaHelper) extends InfoDataProvider {
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
    rdp.find(uoa).flatMap{ jv =>
      Helpers.tryo { new ArtifactInfo4Json(uoa, jv.extract[json.ArtifactFile], uoaHelper) }
    }
  }
  
  def toPackageInfo(uoa: Uoa4Package): List[Box[PackageInfo]] = {
    rdp.find(uoa) match {
      case x: Failure => List(x)
      case Empty => Nil
      case Full(jv) => {
        val pkgFile = jv.extract[json.PkgFile]
        for (tj <- pkgFile.e) yield { Helpers.tryo { new PackageInfo4Json(uoa, tj, this) } }
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

  /**
   * find all artifacts from uoa (including uoa), recursivly, without duplicate.
   * @param uoa root uoa to scan
   * @return the list of result about ArtifactInfo searched
   */
  private def findAllArtifacts(uoa: Uoa4Artifact): List[Box[ArtifactInfo]] = {
    // recursiv collect (manage cycle, avoid double deep search,....)
    def collectRecursiv(uoa: Uoa4Artifact , done : (Set[Uoa4Artifact], List[Box[ArtifactInfo]])) : (Set[Uoa4Artifact], List[Box[ArtifactInfo]]) = {
      done._1.contains(uoa) match {
        case true => done
        case false => {
          val bai = toArtifactInfo(uoa)
          val newDone = (done._1 + uoa, done._2 :+ bai)
          bai.map(_.artifacts.foldLeft(newDone){ (result, u) => collectRecursiv(u,  result) }).openOr(newDone)
        }
      }
    }
    collectRecursiv(uoa, (Set.empty, Nil))._2
  }

  def findAllTypes(uoa: Uoa4Artifact): List[Box[Uoa4Type]] = {
    val art = findAllArtifacts(uoa);
    println("art :" + art)
    art.flatMap( x => x match {
      case f :Failure => List(f)
      case Empty => Nil
      case Full(x) => findAllInnerTypes(Uoa4Package("_root_", x.uoa))
    })
  }
  
  private def findAllInnerTypes(uoa: Uoa4Package): List[Box[Uoa4Type]] = {
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
                case u: Uoa4Package => findAllInnerTypes(u)
                case u: Uoa4Type => Full(u) :: findAllInnerTypes(u)
                case x => println("found :" + x); Nil //ignore
              }
            case x: Failure => List(x)
            case Empty => List(Empty)
          }
        }
      }
    }
  }
  private def findAllInnerTypes(uoa: Uoa4Type, deep : Int = 2): List[Box[Uoa4Type]] = {
    if (deep <= 0) {
      Nil
    } else {
      rdp.find(uoa) match {
        case x: Failure => List(x)
        case Empty => Nil
        case Full(jv) => {
          val tpeFile = jv.extract[json.TpeFile]
          //.map(x => if (excludeObjectSuffix) removeObjectSuffix(x) else x)
          tpeFile.e.flatMap(_.templates).distinct.flatMap { refPath =>
            uoaHelper(refPath) match {
              case Full(uoa) =>
                uoa match {
                  case u: Uoa4Type => Full(u) :: findAllInnerTypes(u, deep - 1)
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

  def toBoxUoa(v: Option[String]): Box[Uoa] = v match {
    case None => Empty
    case Some(v) => uoaHelper(v)
  }
  
  def toListSWTR(s: List[List[String]]): List[StringWithTypeRef] = s.map(x => StringWithTypeRef(x.head, toBoxUoa(x.tail.headOption)))

  def toListFieldext(s : List[String]) : List[Uoa4Fieldext] = {
    for(
        refPath <- s;
        uoa <- uoaHelper(refPath) //ignore failure and empty
        if (uoa.isInstanceOf[Uoa4Fieldext])
      ) yield uoa.asInstanceOf[Uoa4Fieldext]
  }

  def toListType(s : List[String]) : List[Uoa4Type] = {
    for(
        refPath <- s;
        uoa <- uoaHelper(refPath) //ignore failure and empty
        if (uoa.isInstanceOf[Uoa4Type])
      ) yield uoa.asInstanceOf[Uoa4Type]
  }

  def toListPackage(s : List[String]) : List[Uoa4Package] = {
    for(
        refPath <- s;
        uoa <- uoaHelper(refPath) //ignore failure and empty
        if (uoa.isInstanceOf[Uoa4Package])
      ) yield uoa.asInstanceOf[Uoa4Package]
  }
//  def toDocTags(l : List[(String, List[String], Option[String])]) : Seq[DocTag] = {
//    l.map{ e => DocTag4Json(e._1, e._2, e._3) }
//  }
}

object json {

  type RawSplitStringWithRef = List[List[String]]

  case class ArtifactFile(
    artifactId : String,
    version : String,
    description : String,
    groupId : Option[String],
    kind : Option[String],
    logo : Option[String],
    license : Option[String],
    tags : Option[String],
    artifacts : List[String],
    dependencies : List[String]
  )

  case class DocTag(k : String, b : List[String], v : Option[String])
  case class PkgFile(uoa: String, e: List[Pkg])
  case class Pkg(
    name: String,
    qualifiedName: String,
    description: Option[String],
    docTags : List[DocTag],
    templates: List[String],
    packages: List[String])
  case class TpeFile(uoa: String, e: List[Tpe])
  case class Tpe(
    name: String,
    qualifiedName: String,
    description: Option[String],
    docTags : List[DocTag],
    visibility: RawSplitStringWithRef,
    templates: List[String],
    aliasTypes: List[String],
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
    docTags : List[DocTag],
    visibility: RawSplitStringWithRef,
    resultType: RawSplitStringWithRef,
    valueParams: RawSplitStringWithRef,
    typeParams: RawSplitStringWithRef,
    kind: String)
}

//TODO provide converter to json to avoid intermediary type
class ArtifactInfo4Json(val uoa: Uoa4Artifact, src: json.ArtifactFile, uoaHelper: UoaHelper) extends ArtifactInfo {
  override def groupId = src.groupId.getOrElse("")
  override def artifactId = src.artifactId
  override def version = src.version
  override def description = src.description
  override def tags = src.tags.getOrElse("")
  override def logo = src.logo.getOrElse("")
  override def kind = src.kind.getOrElse("")
  override def license = src.license.getOrElse("")
  override val artifacts = src.artifacts.flatMap(x => uoaHelper(x)).map(_.asInstanceOf[Uoa4Artifact])
  override val dependencies = src.dependencies.flatMap(x => uoaHelper(x)).map(_.asInstanceOf[Uoa4Artifact])
  
  override def equals(o : Any) = {
    o match {
      case x : ArtifactInfo => this.uoa == x.uoa
      case _ => false
    }
  }
  
  override def hashCode() = uoa.hashCode

}

case class DocTag4Json(key : String, override val bodies : List[String], override val variant : Option[String]) extends DocTag {
  def this(src : json.DocTag) = this(src.k, src.b, src.v)
}

class PackageInfo4Json(val uoa : Uoa4Package, val src : json.Pkg, rdti : InfoDataProvider0) extends PackageInfo {
  def simpleName: String = src.name
  def description: HtmlString = src.description.getOrElse("")
  val docTags: Seq[DocTag] = src.docTags.map(x => new DocTag4Json(x))
  def source: Option[URI] = None //for (file <- src.sourceStartPoint.headOption ; line <- src.sourceStartPoint.tail.headOption) yield {new URI("src://" + file + "#" + line) }
  def packages: List[Uoa4Package] = rdti.toListPackage(src.packages)
  def types: List[Uoa4Type] = rdti.toListType(src.templates)
}

class TypeInfo4Json(val uoa: Uoa4Type, val src: json.Tpe, rdti : InfoDataProvider0) extends TypeInfo {

  def isCaseClass: Boolean = src.parentType.exists(x => x.head == "Product")
  def simpleName: String = src.name
  def description: HtmlString = src.description.getOrElse("")
  val docTags: Seq[DocTag] = src.docTags.map(x => new DocTag4Json(x))
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
  def fields: List[Uoa4Fieldext] = rdti.toListFieldext(src.values) // TODO includes object (instead of being into types)
  def methods: List[Uoa4Fieldext] = rdti.toListFieldext(src.methods)
  def types: List[Uoa4Type] = rdti.toListType(src.templates)
}

class FieldextInfo4Json(val uoa: Uoa4Fieldext, val src: json.Fieldext, rdti : InfoDataProvider0) extends FieldextInfo {

  def simpleName: String = src.name
  def description: HtmlString = src.description.getOrElse("")
  def docTags: Seq[DocTag] = src.docTags.map(x => new DocTag4Json(x))
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
class RawDataProvider0(workdir : File, apis : ApiService, uoaHelper : UoaHelper) extends RawDataProvider {
  import dispatch._
  import Http._

  private val localApisDir = new File(workdir, "apis")

  def find(uoa: Uoa): Box[JValue] = {
    toUrl(uoa).flatMap { uri =>
        Helpers.tryo{
          uri.getScheme match {
            case "file" => JsonParser.parse(new FileReader(uri.getPath))
            case "local" => JsonParser.parse(new FileReader(toLocalFile(uoa).open_!))
            case "http" => new Http()(new Request(uri.toString) >- { s => JsonParser.parse(s)})
            case x => throw new MalformedURLException("scheme " + x + "is nor supported as source for api (" + uri +")" )
          }
        }
    }
  }

  protected def vscaladoc2Rai(uoa : Uoa) = {
    import net_alchim31_vscaladoc2_www.model.VScaladoc2
    val art = uoaHelper.toUoa4Artifact(uoa)
    apis.findApiOf(art.artifactId, art.version).flatMap { rai =>
        rai.provider match {
            case VScaladoc2 => Full(rai)
            case _ => Failure("data could only be merge from VScaladoc2 source : " + uoa + " is in format " + rai.provider + " located at " + rai.baseUrl)
        }
      }
  }
  
  //TODO handle rai.baseUrl that point to archive instead of directory
  protected def toUrl(uoa: Uoa): Box[URI] = {
      uoa match {
        case Uoa4Artifact(artifactId, version) => {
          for (
            rai <- vscaladoc2Rai(uoa);
            rpath <- rai.provider.rurlPathOf()
          ) yield new URI(rai.baseUrl.toString + rpath)
        }
        case Uoa4Package(packageName, Uoa4Artifact(artifactId, version)) => {
          for (
            rai <- vscaladoc2Rai(uoa);
            rpath <- rai.provider.rurlPathOf(packageName)
          ) yield new URI(rai.baseUrl.toString + rpath)
        }
        case Uoa4Type(typeName, Uoa4Package(packageName, Uoa4Artifact(artifactId, version))) => {
          for (
            rai <- vscaladoc2Rai(uoa);
            rpath <- rai.provider.rurlPathOf(packageName, typeName)
          ) yield new URI(rai.baseUrl.toString + rpath)
        }
        case Uoa4Fieldext(fieldextName, Uoa4Type(typeName, Uoa4Package(packageName, Uoa4Artifact(artifactId, version)))) => {
          for (
            rai <- vscaladoc2Rai(uoa);
            rpath <- rai.provider.rurlPathOf(packageName, typeName, fieldextName)
          ) yield new URI(rai.baseUrl.toString + rpath)
        }
      }
  }

  protected def toLocalFile(rpath : String) : File = new File(localApisDir, rpath).getCanonicalFile

  protected def toLocalFile(uoa: Uoa): Box[File] = {
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

class RawDataProviderWithLocalFSCache(fs : FileSystemHelper, workdir : File, apis : ApiService, uoaHelper : UoaHelper) extends RawDataProvider0(workdir, apis, uoaHelper) with Loggable {
  import dispatch._
  import Http._
  import net.liftweb.actor._
  import java.io.FileOutputStream
  
  private lazy val _archiveDownloader = new ArchiveDownloader()
  
  override def find(uoa: Uoa): Box[JValue] = {
    toUrl(uoa).flatMap { uri =>
        Helpers.tryo{
          uri.getScheme match {
            case "file" => JsonParser.parse(new FileReader(uri.getPath))
            case "local" => JsonParser.parse(new FileReader(toLocalFile(uoa).open_!))
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
   * @todo handle case when base url point to archive
   */
  private def requestFromHttp(uoa : Uoa, uri : URI) : String = {
    toLocalFile(uoa).map { f =>
      if (f.exists) {
        fs.toString(f)
      } else {
        val archive = localArchiveOf(uoa)
        if (archive._2.exists) {
          throw new Exception_UoaNotAvailable(uoa, "not in archives")
        } else {
          _archiveDownloader ! archive._1
          new Http()(new Request(uri.toString) >- { s =>
            val dir = f.getParentFile  
            if (dir.exists || dir.mkdirs()) {   
              fs.toFile(f, s)
            }
            s 
          })
        }
      }
    } openOr {
        new Http()(new Request(uri.toString) as_str)
    }
  }
  
    
  private def localArchiveOf(uoa : Uoa) : (Uoa4Artifact, File) = {
    val artifact = uoaHelper.toUoa4Artifact(uoa)
    (artifact, toLocalFile(rpathOfArchive(artifact)))
  }

  protected def rpathOfArchive(uoa : Uoa4Artifact) = uoa.artifactId + "/" + uoa.version+ "-apidoc.jar.gz"
  
  /**
   * Using one actor to download in background and sequentially required archives of api
   */
  class ArchiveDownloader extends LiftActor {
    protected def messageHandler = {
      case artifact : Uoa4Artifact => {
        val archiveRPath = rpathOfArchive(artifact)
        val dest = toLocalFile(archiveRPath)
        if (dest.exists) {
          //already downloaded by previous request (probably done when file didn't exists)
        } else {
          val tmpdir = toLocalFile("tmp-downloading-" + System.currentTimeMillis)
          val f = new File(tmpdir, archiveRPath)
          f.getParentFile.mkdirs
          // download to the final directory
          vscaladoc2Rai(artifact) match {
            case Full(rai) => {
              val uri = rai.baseUrl.toString match {
                case x if x.endsWith("-apidoc.jar.gz") => x
                case x => x + "-apidoc.jar.gz"
              }
              logger.info("download : " + uri)
              fs.using(new FileOutputStream(f)) { is =>
                new Http()(new Request(uri) >>> is)
              }
              // unarch
              fs.unjar0gz(tmpdir, f)
              //move only files related to the version
              commit(tmpdir, toLocalFile("."), archiveRPath, (artifact.artifactId + "/" + artifact.version), (artifact.artifactId + "/" + artifact.version + "_.json"))
            }
            case _ => commit(tmpdir, toLocalFile("."))
          }
        }
      }
      case _ => () //ignore
    }
    
    private def commit(srcdir : File, destdir : File, filenames : String*) {
      for (fname <- filenames) {
        val dest = new File(destdir, fname)
        if (dest.exists) {
          fs.deleteRecursively(dest)
        }
        fs.move(new File(srcdir, fname), dest)
      }
      fs.deleteRecursively(srcdir)
      if (srcdir.exists) {
        logger.info("can't delete workingd dir : " + srcdir)
      }
    }
  }
  
}

class Exception_UoaNotAvailable(val uoa : Uoa, msg2 : String = "") extends Exception("not available ( " + msg2 + " ) : " + uoa)
