package net_alchim31_vscaladoc2_www

import java.net.URI

object info {
  type Scope = String
  type HtmlString = String

  trait Helper {
    def urlOf(v: String): String
    def urlOf(v: URI): String
    def labelOf(v: String): String
    def labelOf(v: URI): String
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
  }

  trait EntityInfo {
    def url: URI
    def simpleName: String
    def signature: String
    def description: HtmlString
    def docTags: Seq[DocTag]
    def source: Option[URI]
  }
  trait TypeInfo extends EntityInfo {
    def urlOfPackage: URI
    def urlOfArtifact: URI
    def fqName: String
    def kind: String
    def companion: Option[TypeInfo]
    def isInherited(m: MemberInfo)
    def constructors: List[MemberInfo]
    def fields: List[MemberInfo]
    def methods: List[MemberInfo]
  }

  trait MemberInfo extends EntityInfo {}

  trait DocTag {
    def label: String
    def key: String = ""
    def body: String
  }
}
