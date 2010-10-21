package net_alchim31_vscaladoc2_www

import net.liftweb.common.{ Box, Full, Empty, Failure }
import net.liftweb.json.JsonAST.JObject
import java.net.URI

/**
 * Definition of all types (data and service) available from template
 *
 * @author david.bernard
 */
object info {
  type Scope = String
  type HtmlString = String


  sealed trait Uoa
  case class Uoa4Artifact(artifactId: String, version: String) extends Uoa
  case class Uoa4Package(packageName: String, uoaArtifact: Uoa4Artifact) extends Uoa
  case class Uoa4Type(typeName: String, uoaPackage: Uoa4Package) extends Uoa
  case class Uoa4Fieldext(fieldextName: String, uoaType: Uoa4Type) extends Uoa

  trait Helper {
    def urlOf(v: String): String
    def urlOf(v : Uoa, subContext : String = "") : String
    def labelOf(v: String): String
    def labelOf(v: Uoa): String
    def fqNameOf(v : Uoa) : String
  }

  trait ArtifactKind
  object ArtifactKind {
    case object Undef extends ArtifactKind
    case object Group extends ArtifactKind
    case object Jar extends ArtifactKind
    case class Plugin(platform: String) extends ArtifactKind
    case object War extends ArtifactKind
    case object OSGiBundle extends ArtifactKind
  }

  trait ArtifactInfo {
    def logo: Option[URI] = None
    def groupId: String = ""
    def artifactId: String = ""
    def version: String = ""
    def description: HtmlString = ""
    def homeUrl: Option[URI] = None
    def apiUrl: Option[URI] = None
    def licences: List[String] = Nil
    def kind: ArtifactKind = ArtifactKind.Undef
    def groups: List[ArtifactInfo] = Nil
    def dependencies: List[(ArtifactInfo, Scope)] = Nil
    //def rawjson : Box[JObject] = Empty
  }

  trait EntityInfo {
    def simpleName: String
    def signature: String
    def description: HtmlString
    def docTags: Seq[DocTag]
    def source: Option[URI]
    //def rawjson : Box[JObject] = Empty
  }

  trait TypeInfo extends EntityInfo {
    def uoa: Uoa4Type
    def kind: String
    def isInherited(m: FieldextInfo)
    def constructors: List[FieldextInfo]
    def fields: List[FieldextInfo]
    def methods: List[FieldextInfo]
  }

  trait FieldextInfo extends EntityInfo {
    def uoa: Uoa4Fieldext
  }

  trait DocTag {
    def label: String
    def key: String = ""
    def body: String
  }
}

class UoaHelper() {
  import info._

  def apply(refPath: String): Box[Uoa] = {
    val fragments = refPath.split('/').toList
    fragments match {
      case artifactId :: version :: packageName :: Nil => Full(Uoa4Package(packageName, Uoa4Artifact(artifactId, version)))
      case artifactId :: version :: packageName :: typeName :: Nil => Full(Uoa4Type(typeName,Uoa4Package(packageName, Uoa4Artifact(artifactId, version))))
      case artifactId :: version :: packageName :: typeName :: fieldextName :: Nil => Full(Uoa4Fieldext(fieldextName, Uoa4Type(typeName,Uoa4Package(packageName, Uoa4Artifact(artifactId, version)))))
      case _ => Failure("refPath don't match the uoa format artifactId/version/packageName[/typeName[/memberName]] : " + refPath)
    }
  }

  def toRefPath(uoa: Uoa): String = uoa match {
    case Uoa4Artifact(artifactId, version) => artifactId + "/" + version
    case Uoa4Package(packageName, uoaArtifact) => toRefPath(uoaArtifact) + "/" + packageName
    case Uoa4Type(typeName, uoaPackage) => toRefPath(uoaPackage) + "/" + typeName
    case Uoa4Fieldext(fieldextName, uoaType) => toRefPath(uoaType) + "/" + fieldextName
  }
}
