package net_alchim31_vscaladoc2_www

import net_alchim31_vscaladoc2_www.model.RemoteApiInfo
import net.liftweb.common.Empty
import java.net.URL
import net.liftweb.common._
import net.liftweb.util.Helpers.tryo
import _root_.net.liftweb.http._
import java.util.Date
import net_alchim31_vscaladoc2_www.info._

trait PartDisplayer {
  def serveResource(path: List[String], ext : String): Box[LiftResponse]
}

trait NavigatorDisplayer extends PartDisplayer {
  def serveNavigator(api : RemoteApiInfo, entityPath : List[String]) : Box[LiftResponse]
  def serveBrowser(api : RemoteApiInfo, entityPath : List[String]) : Box[LiftResponse]
}

trait EntityDisplayer extends PartDisplayer {
  def serveFieldext(uoa : Uoa4Fieldext): Box[LiftResponse]
  def serveType(uoa : Uoa4Type): Box[LiftResponse]
  def servePackage(uoa : Uoa4Package): Box[LiftResponse]
  def serveArtifact(uoa : Uoa4Artifact): Box[LiftResponse]
  def serveArtifacts(artifactId : String): Box[LiftResponse]
}

trait SourceDisplayer extends PartDisplayer {
  def serveHtmlized(api : RemoteApiInfo, entityPath : List[String], ext : String, lineNum : Box[Int]) : Box[LiftResponse]
}

class EntityDisplayer4Debug extends EntityDisplayer {
  def serveResource(path: List[String], ext : String): Box[LiftResponse] = Full(XmlResponse(
    <div>part :{ path.mkString("/") + ext }</div>
    ))
  def serveFieldext(uoa : Uoa4Fieldext): Box[LiftResponse] = Full(XmlResponse(
    <div>Fieldext :{ uoa }</div>
    ))
  def serveType(uoa : Uoa4Type): Box[LiftResponse] = Full(XmlResponse(
    <div>Type :{ uoa }</div>
    ))
  def servePackage(uoa : Uoa4Package): Box[LiftResponse] = Full(XmlResponse(
    <div>Package :{ uoa }</div>
    ))
  def serveArtifact(uoa : Uoa4Artifact): Box[LiftResponse] = Full(XmlResponse(
    <div>Artifact :{ uoa }</div>
    ))
  def serveArtifacts(artifactId: String): Box[LiftResponse] = Full(XmlResponse(
    <div>Artifacts :{ artifactId }</div>
    ))
}
