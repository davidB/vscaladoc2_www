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
import net.sf.ehcache.{Cache, Element}

trait RawDataProvider {
  type UOA = String
  def find(uoa: Uoa): Box[JValue]
  def toSourceString(uoa: Uoa4Artifact, filePath : String) : Box[String] = Failure("not implemented")
}

trait InfoDataProvider {
  def toArtifactInfo(uoa: Uoa4Artifact): Box[ArtifactInfo] = Empty
  def toPackageInfo(uoa: Uoa4Package): List[Box[PackageInfo]] = Nil
  def toTypeInfo(uoa: Uoa4Type): List[Box[TypeInfo]] = Nil
  def toFieldextInfo(uoa: Uoa4Fieldext): List[Box[FieldextInfo]] = Nil
  def findAllNonEmptyPackages(uoa: Uoa4Artifact): List[Box[Uoa4Package]] = Nil
  def findAllTypes(uoa: Uoa4Artifact): List[Box[Uoa4Type]] = Nil
  def toSourceString(uoa: Uoa4Artifact, filePath : List[String], ext : String) : Box[String] = Failure("not implemented")
}

class ApiService(lazy_idp : () => InfoDataProvider, cache2rai : Cache) extends Loggable {
  import net_alchim31_vscaladoc2_www.model.{RemoteApiInfo, ApiProviders, VScaladoc2}

  import net.liftweb.actor._
    
  private lazy val idp = lazy_idp()
  private lazy val _childrenRegistor = new ChildrenRegistor()
  
  def init() {
    if (RemoteApiInfo.find() == Empty){
    val data = List(
        //("vscaladoc2_demoprj", "0.1-SNAPSHOT", new URI("local:/"), ApiProviders.vscaladoc2),
        //("vscaladoc2_demoprj", "0.1-SNAPSHOT", new URI("http://davidb.github.com/vscaladoc2_demoprj/vscaladoc2_demoprj/0.1-SNAPSHOT"), ApiProviders.vscaladoc2),
        ("vscaladoc2_demoprj", "0.1-SNAPSHOT", new URI("http://alchim31.free.fr/apis/vscaladoc2_demoprj/0.1-SNAPSHOT"), ApiProviders.vscaladoc2, Some("vscaladoc")),
        //("framework_2.8.0", "2.2-M1", new URI("local:/"), ApiProviders.vscaladoc2)
        ("scala-library", "2.8.0", new URI("http://alchim31.free.fr/apis/scala-library/2.8.0"), ApiProviders.vscaladoc2, None),
        ("scala-library", "2.7.7", new URI("http://www.scala-lang.org/api/2.7.7"), ApiProviders.scaladoc, None)
    ).foreach { x =>
        val v: RemoteApiInfo = RemoteApiInfo.create
        v.artifactId(x._1)
        v.version(x._2)
        v.url(x._3.toASCIIString)
        v.format(x._4)
        x._5.foreach( x => v.ggroupId(x))
        register(v)
      }
    }
  }
  
  def findApiOf(artifactId: String, version: String): Box[RemoteApiInfo] = {
    val k = artifactId + "::"+ version
    def update() = {
      val v = RemoteApiInfo.findApiOf(artifactId, version)
      cache2rai.put(new Element(k, v))
      v
    }
    cache2rai.get(k) match {
      case null => update() 
      case elem => elem.getObjectValue match {
        case r : Box[RemoteApiInfo] => r 
        case _ => update()
      }
    }
  }

  def register(v : RemoteApiInfo) = {
    v.save
    _childrenRegistor ! v
  }
  
  class ChildrenRegistor extends LiftActor {
    protected def messageHandler = {
      case v : RemoteApiInfo => registerChildren(v)
      case _ => () //ignore
    }
    
    private def registerChildren(v : RemoteApiInfo) {
      // TODO check if the remote api is available or not
      logger.info("register : " + v)
      v.provider match {
        case VScaladoc2 => {
          val bartifact = idp.toArtifactInfo(Uoa4Artifact(v.artifactId.is, v.version.is))
          val children = bartifact.map(_.artifacts).openOr(Nil)
          children.foreach { uoa =>
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
                v2.ggroupId(v.ggroupId)
                v2.save
                this ! v2
              }
              case _ => () //ignore
            }
          }
        }
        case _ => () //ignore
      }
    }
  }
}

trait InfoDataProviderCache extends InfoDataProvider {
  self : InfoDataProvider =>

  protected def cacheUoa2info : Cache
  protected def cacheUoa2types : Cache
  
  override def toArtifactInfo(uoa: Uoa4Artifact): Box[ArtifactInfo] = findOrUpdate(cacheUoa2info, uoa)(super.toArtifactInfo)
  override def toPackageInfo(uoa: Uoa4Package): List[Box[PackageInfo]] = findOrUpdate(cacheUoa2info, uoa)(super.toPackageInfo)
  override def toTypeInfo(uoa: Uoa4Type): List[Box[TypeInfo]] = findOrUpdate(cacheUoa2info, uoa)(super.toTypeInfo) 
  override def toFieldextInfo(uoa: Uoa4Fieldext): List[Box[FieldextInfo]] = findOrUpdate(cacheUoa2info, uoa)(super.toFieldextInfo) 
  
  override def findAllTypes(uoa: Uoa4Artifact): List[Box[Uoa4Type]] = findOrUpdate(cacheUoa2types, uoa)(super.findAllTypes)
  
  private def findOrUpdate[KeyType, ResultType](cache : Cache, k : KeyType)(f : KeyType => ResultType )(implicit m : Manifest[ResultType]) : ResultType = {
    def update() = {
      val v = f(k)
      cache.put(new Element(k, v))
      v
    }
    cache.get(k) match {
      case null => update() 
      case elem => {
        val o = elem.getObjectValue 
        if (m.erasure.isInstance(o)) {
          o.asInstanceOf[ResultType]
        } else {
          update()
        }
      }
    }
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
  val sourceRangeFormat = """([a-zA-Z0-9_\$./]+)(#(\d+)(:(\d+))?(-(\d+)(:(\d+))?)?)?""".r
  override def toArtifactInfo(uoa: Uoa4Artifact): Box[ArtifactInfo] = {
    rdp.find(uoa).flatMap{ jv =>
      Helpers.tryo {
        import model.{RemoteApiInfo}
        val ggroupId = RemoteApiInfo.findApiOf(uoa.artifactId, uoa.version).map(_.ggroupId.is).toOption //TODO remove RemoteApiInfo object dependency
        new ArtifactInfo4Json(uoa, jv.extract[json.ArtifactFile], uoaHelper, this, ggroupId)
      }
    }
  }
  
  override def toPackageInfo(uoa: Uoa4Package): List[Box[PackageInfo]] = {
    rdp.find(uoa) match {
      case x: Failure => List(x)
      case Empty => Nil
      case Full(jv) => {
        val pkgFile = jv.extract[json.PkgFile]
        for (tj <- pkgFile.e) yield { Helpers.tryo { new PackageInfo4Json(uoa, tj, this) } }
      }
    }
  }
  
  override def toTypeInfo(uoa: Uoa4Type): List[Box[TypeInfo]] = {
    rdp.find(uoa) match {
      case x: Failure => List(x)
      case Empty => Nil
      case Full(jv) => {
        val tpeFile = jv.extract[json.TpeFile]
        for (tj <- tpeFile.e) yield { Helpers.tryo { new TypeInfo4Json(uoa, tj, this) } }
      }
    }
  }

  override def toFieldextInfo(uoa: Uoa4Fieldext): List[Box[FieldextInfo]] = {
    rdp.find(uoa) match {
      case x: Failure => List(x)
      case Empty => Nil
      case Full(jv) => {
        val file = jv.extract[json.FieldextFile]
        for (entry <- file.e) yield { Helpers.tryo { new FieldextInfo4Json(uoa, entry, this) } }
      }

    }
  }

  override def toSourceString(uoa: Uoa4Artifact, filePath : List[String], ext : String) : Box[String] = {
    val p = filePath.mkString("", "/", ext)
    rdp.toSourceString(uoa, p.substring(0, p.length-".html".length))
  }
  
  /**
   * find all artifacts from uoa (including uoa), recursivly, without duplicate.
   * @param uoa root uoa to scan
   * @return the list of result about ArtifactInfo searched
   */
  private def findAllArtifacts(uoa: Uoa4Artifact): List[Box[ArtifactInfo]] = {
    // recursiv collect (manage cycle, avoid double deep search, ...)
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

  override def findAllNonEmptyPackages(uoa: Uoa4Artifact): List[Box[Uoa4Package]] = {
    val art = findAllArtifacts(uoa);
    art.flatMap( x => x match {
      case f :Failure => List(f)
      case Empty => Nil
      case Full(x) => findAllInnerNonEmptyPackages(Uoa4Package("_root_", x.uoa))
    })
  }
  
  def findAllInnerNonEmptyPackages(ruoa: Uoa4Package, includeItself : Boolean = true): List[Box[Uoa4Package]] = {
    rdp.find(ruoa) match {
      case x: Failure => List(x)
      case Empty => Nil
      case Full(jv) => {
        val pkgFile = jv.extract[json.PkgFile]
        //.map(x => if (excludeObjectSuffix) removeObjectSuffix(x) else x)
        pkgFile.e.flatMap { pkg =>
          val children = pkg.packages.flatMap { refPath =>
            uoaHelper(refPath) match {
              case Full(uoa) =>
                uoa match {
                  case u: Uoa4Package => findAllInnerNonEmptyPackages(u)
                  case x => Nil //ignore
                }
              case x: Failure => List(x)
              case Empty => Nil//List(Empty)
            }
          }
          (includeItself && (pkg.templates.length != pkg.packages.length)) match {
            case false => children
            case true => Full(ruoa) :: children
          }
        }
      }
    }
  }
  
  override def findAllTypes(uoa: Uoa4Artifact): List[Box[Uoa4Type]] = {
    val art = findAllArtifacts(uoa);
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
        val children = pkgFile.e.flatMap(_.templates).distinct
        children.flatMap { refPath =>
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
      //select only not inherited types
      val refPathPrefix = uoaHelper.toRefPath(uoa) + "."
      rdp.find(uoa) match {
        case x: Failure => List(x)
        case Empty => Nil
        case Full(jv) => {
          val tpeFile = jv.extract[json.TpeFile]
          //.map(x => if (excludeObjectSuffix) removeObjectSuffix(x) else x)
          val children = tpeFile.e.flatMap(_.templates).filter(_.startsWith(refPathPrefix)).distinct
          children.flatMap { refPath =>
            uoaHelper(refPath) match {
              case Full(uoaChild) =>
                uoaChild match {
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

  def toSourceRange(s : Option[String]) : Option[SourceRange] = s.flatMap { s =>
    def toSourcePos(l : String, c : String) : Option[SourcePos] = (l, c) match {
      case (null, _) => None
      case (line, null) => Some(SourcePos(line.toInt, None))
      case (line, col) => Some(SourcePos(line.toInt, Some(col.toInt)))
    }
    s match {
      case sourceRangeFormat(path, _, beginLine, _, beginCol, _, endLine, _, endCol) => {
        var begin : Option[SourcePos] =  toSourcePos(beginLine, endLine)
        var end : Option[SourcePos] = toSourcePos(endLine, endCol)
        Some(SourceRange(path, begin, end))
      }
      case _ =>     println("don't match sourceRangeFormat", s); None
    }
  }

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
    linksources : Option[String],
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
    source : Option[String],
    templates: List[String],
    packages: List[String])
  case class TpeFile(uoa: String, e: List[Tpe])
  case class Tpe(
    name: String,
    qualifiedName: String,
    description: Option[String],
    docTags : List[DocTag],
    source : Option[String],
    visibility: RawSplitStringWithRef,
    templates: List[String],
    aliasTypes: List[String],
    //resultType: RawSplitStringWithRef, // [ "DemoB", "vscaladoc_demoprj/0.1-SNAPSHOT/itest.demo2/DemoB" ] ],
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
    source : Option[String],
    visibility: RawSplitStringWithRef,
    resultType: RawSplitStringWithRef,
    valueParams: RawSplitStringWithRef,
    typeParams: RawSplitStringWithRef,
    kind: String)
}

//TODO provide converter to json to avoid intermediary type
@serializable
class ArtifactInfo4Json(val uoa: Uoa4Artifact, src: json.ArtifactFile, uoaHelper: UoaHelper, rdti : InfoDataProvider, override val ggroupId : Option[String]) extends ArtifactInfo {
  override def groupId = src.groupId.getOrElse("")
  override def artifactId = src.artifactId
  override def version = src.version
  override def description = src.description
  override def tags = src.tags.getOrElse("")
  override def logo = src.logo.getOrElse("")
  override def kind = src.kind.getOrElse("")
  override def license = src.license.getOrElse("")
  override def linksources = src.linksources
  override lazy val artifacts = src.artifacts.flatMap(x => uoaHelper(x)).map(_.asInstanceOf[Uoa4Artifact])
  override lazy val dependencies = src.dependencies.flatMap(x => uoaHelper(x)).map(_.asInstanceOf[Uoa4Artifact])
  override lazy val packages = for(buoa <- rdti.findAllNonEmptyPackages(uoa); uoa <- buoa) yield uoa // TODO not ignore error
  override def equals(o : Any) = {
    o match {
      case x : ArtifactInfo => this.uoa == x.uoa
      case _ => false
    }
  }
  
  override def hashCode() = uoa.hashCode

}

@serializable
case class DocTag4Json(key : String, override val bodies : List[String], override val variant : Option[String]) extends DocTag {
  def this(src : json.DocTag) = this(src.k, src.b, src.v)
}

@serializable
class PackageInfo4Json(val uoa : Uoa4Package, val src : json.Pkg, rdti : InfoDataProvider0) extends PackageInfo {
  def simpleName: String = src.name
  def description: HtmlString = src.description.getOrElse("")
  lazy val docTags: Seq[DocTag] = src.docTags.map(x => new DocTag4Json(x))
  def source: Option[SourceRange] = rdti.toSourceRange(src.source)
  lazy val packages: List[Uoa4Package] = for(buoa <- rdti.findAllInnerNonEmptyPackages(uoa, false); uoa <- buoa) yield uoa // TODO not ignore error //rdti.toListPackage(src.packages)
  lazy val types: List[Uoa4Type] = rdti.toListType(src.templates)
}

@serializable
class TypeInfo4Json(val uoa: Uoa4Type, val src: json.Tpe, rdti : InfoDataProvider0) extends TypeInfo {

  def isCaseClass: Boolean = src.parentType.exists(x => x.head == "Product")
  def simpleName: String = src.name
  def description: HtmlString = src.description.getOrElse("")
  lazy val docTags: Seq[DocTag] = src.docTags.map(x => new DocTag4Json(x))
  def source: Option[SourceRange] = rdti.toSourceRange(src.source)
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

@serializable
class FieldextInfo4Json(val uoa: Uoa4Fieldext, val src: json.Fieldext, rdti : InfoDataProvider0) extends FieldextInfo {

  def simpleName: String = src.name
  def description: HtmlString = src.description.getOrElse("")
  def docTags: Seq[DocTag] = src.docTags.map(x => new DocTag4Json(x))
  def source: Option[SourceRange] = rdti.toSourceRange(src.source)
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
  private val _archiveSuffix = "-apidoc.jar.gz"
  
  override def find(uoa: Uoa): Box[JValue] = {
    toUrl(uoa).flatMap { uri =>
        Helpers.tryo{
          uri.getScheme match {
            case "file" => JsonParser.parse(new FileReader(uri.getPath))
            case "local" => JsonParser.parse(new FileReader(toLocalFile(uoa).open_!))
            case "http" => JsonParser.parse(requestFromHttp(uoa, uri))
            case "https" => JsonParser.parse(requestFromHttp(uoa, uri))
            case x => throw new MalformedURLException("scheme " + x + " is nor supported as source for api (" + uri +")" )
          }
        }
    }
  }

  override def toSourceString(uoa: Uoa4Artifact, filePath : String) : Box[String] = {
    val rpath = uoa.artifactId + "/"+ uoa.version + "/_src_/" + filePath
    logger.info("try to retrieve content of :" + rpath)
    val f = toLocalFile(rpath)
    f.exists match {
      case true => Helpers.tryo{ fs.toString(f) }
      case false => Failure("file '" + rpath + "' not availables (from cache)")
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
        } else /*if (uri.getPath.indexOf(_archiveSuffix) > -1)*/ {
          (_archiveDownloader !?(600*1000, archive._1)).asInstanceOf[Box[Box[Any]]].flatMap(x => x) match {
            case Full(true) => f.exists match {
              case true => fs.toString(f)
              case false => throw new Exception_UoaNotAvailable(uoa, "not in archives")
            }
            case x : Failure => f.exists match {
              case true => fs.toString(f) //timeout could be raised, but file could be available (due to long queue and archive download by a previous request)
              case false =>throw (x.exception openOr new Exception(x.msg)) 
            }
            case Empty => throw new Exception("empty reply to download request")
          }
        }
//        else {
//          _archiveDownloader ! archive._1
//          new Http()(new Request(uri.toString) >- { s =>
//            val dir = f.getParentFile  
//            if (dir.exists || dir.mkdirs()) {   
//              fs.toFile(f, s)
//            }
//            s 
//          })
//        }
      }
    } openOr {
        new Http()(new Request(uri.toString) as_str)
    }
  }
  
    
  private def localArchiveOf(uoa : Uoa) : (Uoa4Artifact, File) = {
    val artifact = uoaHelper.toUoa4Artifact(uoa)
    (artifact, toLocalFile(rpathOfArchive(artifact)))
  }

  protected def rpathOfArchive(uoa : Uoa4Artifact) = uoa.artifactId + "/" + uoa.version + _archiveSuffix
  
  /**
   * Using one actor to download in background and sequentially required archives of api
   */
  class ArchiveDownloader extends LiftActor {
    protected def messageHandler = {
      case artifact : Uoa4Artifact => {
        val archiveRPath = rpathOfArchive(artifact)
        val dest = toLocalFile(archiveRPath)
        val r = if (dest.exists) {
          //already downloaded by previous request (probably done when file didn't exists)
          Full(true)
        } else {
          val tmpdir = toLocalFile("tmp-downloading-" + System.currentTimeMillis)
          val f = new File(tmpdir, archiveRPath)
          f.getParentFile.mkdirs
          // download to the final directory
          vscaladoc2Rai(artifact).map { rai =>
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
            dest.exists
          }
        }
        reply(r)
      }
      case _ => Empty //ignore
    }
    
    private def commit(srcdir : File, destdir : File, filenames : String*) {
      destdir.mkdirs
      for (fname <- filenames) {
        val dest = new File(destdir, fname)
        val src = new File(srcdir, fname)
        if (dest.exists) {
          fs.deleteRecursively(dest)
        }
        if (src.exists) {
          dest.getParentFile.mkdir
          fs.move(new File(srcdir, fname), dest)
        } else {
          logger.warn("source to move doesn't exists : " + src)
        }
      }
      fs.deleteRecursively(srcdir)
      if (srcdir.exists) {
        logger.info("can't delete workingd dir : " + srcdir)
      }
    }
  }
  
}

class Exception_UoaNotAvailable(val uoa : Uoa, msg2 : String = "") extends Exception("not available ( " + msg2 + " ) : " + uoa)
