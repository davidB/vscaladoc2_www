package net_alchim31_vscaladoc2_www

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

  case class StringWithTypeRef(s: String, uoaType : Box[Uoa] = Empty)

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
    def uoa: Uoa4Artifact
    def groupId: String = ""
    def artifactId: String = ""
    def version: String = ""
    def tags: String = ""
    def description: HtmlString = ""
    def logo : HtmlString = ""
    def license : HtmlString = ""
    def kind: ArtifactKind = ArtifactKind.Undef
    def artifacts: List[Uoa4Artifact] = Nil
    def dependencies: List[Uoa4Artifact] = Nil
    //def rawjson : Box[JObject] = Empty
  }

  trait EntityInfo {
    def simpleName: String
    def signature: List[StringWithTypeRef]
    def description: HtmlString
    def docTags: Seq[DocTag]
    def source: Option[URI]
    //def rawjson : Box[JObject] = Empty
    def kind: String
  }

  trait TypeInfo extends EntityInfo {
    def uoa: Uoa4Type
    def isInherited(m: FieldextInfo) : Boolean
    def constructors: List[Box[FieldextInfo]]
    def fields: List[Box[FieldextInfo]]
    def methods: List[Box[FieldextInfo]]
  }

  trait FieldextInfo extends EntityInfo {
    def uoa: Uoa4Fieldext
  }

  trait DocTag {
    def name : String
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

  def apply(refPath: String): Box[Uoa] = apply(refPath.split('/').toList)

  def toRefPath(uoa: Uoa): String = uoa match {
    case Uoa4Artifact(artifactId, version) => artifactId + "/" + version
    case Uoa4Package(packageName, uoaArtifact) => toRefPath(uoaArtifact) + "/" + packageName
    case Uoa4Type(typeName, uoaPackage) => toRefPath(uoaPackage) + "/" + typeName
    case Uoa4Fieldext(fieldextName, uoaType) => toRefPath(uoaType) + "/" + fieldextName
  }
}
