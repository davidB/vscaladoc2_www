package net_alchim31_vscaladoc2_www

import net.liftweb.util.Helpers

/**
 * Definition of all types (data and service) available from template
 *
 * @author david.bernard
 */
object info {
  import net.liftweb.common.{ Box, Full, Empty, Failure }
  import net.liftweb.json.JsonAST.JObject
  import java.net.URI

  type Scope = String
  type HtmlString = String
  type ArtifactKind = String
  
  case class StringWithTypeRef(s: String, uoaType : Box[Uoa] = Empty)
  case class SourcePos(line : Int, column: Option[Int])
  case class SourceRange(path : String, begin : Option[SourcePos], end : Option[SourcePos])
  
  sealed trait Uoa
  case class Uoa4Artifact(artifactId: String, version: String) extends Uoa
  case class Uoa4Package(packageName: String, uoaArtifact: Uoa4Artifact) extends Uoa
  case class Uoa4Type(typeName: String, uoaPackage: Uoa4Package) extends Uoa
  case class Uoa4Fieldext(fieldextName: String, uoaType: Uoa4Type) extends Uoa

  trait Helper {
    def urlOf(v: String): String
    def urlOf(v : Uoa, subContext : String = "") : String
    def refPathOf(uoa: Uoa): String
    def labelOf(v: String): String
    def labelOf(v: Uoa): String
    def fqNameOf(v : Uoa) : String
    def toArtifactInfo(uoa: Uoa4Artifact): Box[ArtifactInfo]
    def toTypeInfo(uoa: Uoa4Type): List[Box[TypeInfo]]
    def toFieldextInfo(uoa: Uoa4Fieldext): List[Box[FieldextInfo]]
    def urlOfSource(entity : EntityInfo) : Option[String] = entity match {
      case entity : PackageInfo  => toArtifactInfo(entity.uoa.uoaArtifact).toOption.flatMap{ x =>  urlOfSource(x, entity) };
      case entity : TypeInfo     => toArtifactInfo(entity.uoa.uoaPackage.uoaArtifact).toOption.flatMap{ x => urlOfSource(x, entity) }
      case entity : FieldextInfo => toArtifactInfo(entity.uoa.uoaType.uoaPackage.uoaArtifact).toOption.flatMap{ x => urlOfSource(x, entity) }
    }
    def urlOfSource(artifact : ArtifactInfo, entity : EntityInfo) : Option[String] = urlOfSource(artifact, entity.source)
    def urlOfSource(artifact : ArtifactInfo, srcRange : Option[SourceRange]) : Option[String] = {
      def interpolate0(text: String, vars: Map[String, String]) : String= {
        (text /: vars) { (t, kv) => t.replace("${"+kv._1+"}", kv._2)  }
      }
      def interpolate(text : String, srcRange : SourceRange): String = interpolate0(text, Map(
        ("artifactId" -> artifact.artifactId),
        ("version" -> artifact.version),
        ("path" -> srcRange.path),
        ("beginLine" -> srcRange.begin.map(_.line.toString).getOrElse(""))
        )
      )
      for (linksources <- artifact.linksources ; srcRange <- srcRange) yield {
        linksources match {
          case "embed:/" => urlOf(interpolate("/laf/src/${artifactId}/${version}/${path}.html#${beginLine}",srcRange))
          case pattern =>  interpolate(pattern, srcRange)
        }
      }
    }
  }

//  trait ArtifactKind
//  object ArtifactKind {
//    case object Undef extends ArtifactKind
//    case object Group extends ArtifactKind
//    case object Jar extends ArtifactKind
//    case class Plugin(platform: String) extends ArtifactKind
//    case object War extends ArtifactKind
//    case object OSGiBundle extends ArtifactKind
//  }

  trait ArtifactInfo {
    def uoa: Uoa4Artifact
    def groupId: String = ""
    def artifactId: String = ""
    def version: String = ""
    def tags: String = ""
    def description: HtmlString = ""
    def logo : HtmlString = ""
    def license : HtmlString = ""
    def kind: ArtifactKind = ""
    def artifacts: List[Uoa4Artifact] = Nil
    def dependencies: List[Uoa4Artifact] = Nil
    def packages: List[Uoa4Package] = Nil
    def ggroupId : Option[String] = None
    def linksources : Option[String] = None
    //def rawjson : Box[JObject] = Empty
  }

  trait EntityInfo {
    type UoaT <: Uoa
    def uoa: UoaT
    def simpleName: String
    def signature: List[StringWithTypeRef]
    def description: HtmlString
    def docTags: Seq[DocTag]
    def source: Option[SourceRange]
    //def rawjson : Box[JObject] = Empty
    def kind: String
  }

  trait PackageInfo extends EntityInfo {
    type UoaT = Uoa4Package
    def signature: List[StringWithTypeRef] = Nil
    def kind: String = "package"
    def packages: List[Uoa4Package]
    def types: List[Uoa4Type]
  }
  
  trait TypeInfo extends EntityInfo {
    type UoaT = Uoa4Type
    def isInherited(m: FieldextInfo) : Boolean
    def constructors: List[Box[FieldextInfo]]
    def fields: List[Uoa4Fieldext]
    def methods: List[Uoa4Fieldext]
    def types: List[Uoa4Type]
  }

  trait FieldextInfo extends EntityInfo {
    type UoaT = Uoa4Fieldext
  }

  trait DocTag {
    def key : String
    def variant : Option[String] = None
    def bodies : List[String] = Nil
  }
}

class UoaHelper() {
  import info._
  import net.liftweb.common.{ Box, Full, Empty, Failure }

  def apply(fragments : List[String]): Box[Uoa] = {
    fragments match {
      case artifactId :: version :: Nil => Full(Uoa4Artifact(artifactId, version))
      case artifactId :: version :: packageName :: Nil => Full(Uoa4Package(packageName, Uoa4Artifact(artifactId, version)))
      case artifactId :: version :: packageName :: typeName :: Nil => Full(Uoa4Type(typeName,Uoa4Package(packageName, Uoa4Artifact(artifactId, version))))
      case artifactId :: version :: packageName :: typeName :: fieldextName :: Nil => Full(Uoa4Fieldext(fieldextName, Uoa4Type(typeName,Uoa4Package(packageName, Uoa4Artifact(artifactId, version)))))
      case _ => Failure("fragments don't match the uoa format artifactId, version, [packageName [, typeName[, memberName]]] : " + fragments.mkString(","))
    }
  }

  def apply(refPath: String): Box[Uoa] = apply(splitRefPath(refPath))

  def toRefPath(uoa: Uoa): String = uoa match {
    case Uoa4Artifact(artifactId, version) => artifactId + "/" + version
    case Uoa4Package(packageName, uoaArtifact) => toRefPath(uoaArtifact) + "/" + packageName
    case Uoa4Type(typeName, uoaPackage) => toRefPath(uoaPackage) + "/" + typeName
    case Uoa4Fieldext(fieldextName, uoaType) => toRefPath(uoaType) + "/" + fieldextName
  }
  
  def toUoa4Artifact(uoa : Uoa) : Uoa4Artifact = uoa match {
    case x : Uoa4Artifact => x
    case Uoa4Package(packageName, uoaArtifact) => uoaArtifact
    case Uoa4Type(typeName, uoaPackage) => uoaPackage.uoaArtifact
    case Uoa4Fieldext(fieldextName, uoaType) => uoaType.uoaPackage.uoaArtifact
  }

  def toUoa4Artifact(refPath : String) : Box[Uoa4Artifact] = Helpers.tryo{
    val l = splitRefPath(refPath) 
    Uoa4Artifact(l(0), l(1))
  }
  
  def splitRefPath(refPath : String) = refPath.split('/').toList.map(_.trim).filter(_.length > 0)
  
  /**
   * replace version by '_'
   * @param refPath refPath to anonymize
   * @return a tuple (extracted version, refPath with anonymized Version)
   */
  def anonymizeVersion(refPath : String) : (String, String) = {
    val l = splitRefPath(refPath)
    val v = l(1)
    val l2 = l.head :: "_" :: l.tail.tail
    (v, l2.mkString("/"))
  }
}
