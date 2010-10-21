package net_alchim31_vscaladoc2_www

import net_alchim31_vscaladoc2_www.model.RemoteApiInfo
import net.liftweb.common.Empty
import java.net.URL
import net.liftweb.common._
import net.liftweb.util.Helpers.tryo
import _root_.net.liftweb.http._
import java.util.Date

trait PartDisplayer {
  def serveResource(path: List[String], ext : String): Box[LiftResponse]
}

trait NavigatorDisplayer extends PartDisplayer {
  def serveNavigator(api : RemoteApiInfo, entityPath : List[String]) : Box[LiftResponse]
  def serveBrowser(api : RemoteApiInfo, entityPath : List[String]) : Box[LiftResponse]
}

trait EntityDisplayer extends PartDisplayer {
  def serveMember(srcUrl: URL): Box[LiftResponse]
  def serveType(rai: RemoteApiInfo, srcUrl: URL): Box[LiftResponse]
  def servePackage(srcUrl: URL): Box[LiftResponse]
  def serveArtifact(rai: RemoteApiInfo, srcUrl: URL): Box[LiftResponse]
  def serveArtifacts(artifactId: String): Box[LiftResponse]
}

class EntityDisplayer4Debug extends EntityDisplayer {
  def serveResource(path: List[String], ext : String): Box[LiftResponse] = Full(XmlResponse(
    <div>part :{ path.mkString("/") + ext }</div>
    ))
  def serveMember(fullUrl: URL): Box[LiftResponse] = Full(XmlResponse(
    <div>Member1 :{ fullUrl }</div>
    ))
  def serveType(rai: RemoteApiInfo, fullUrl: URL): Box[LiftResponse] = Full(XmlResponse(
    <div>Type :{ fullUrl }</div>
    ))
  def servePackage(fullUrl: URL): Box[LiftResponse] = Full(XmlResponse(
    <div>Package :{ fullUrl }</div>
    ))
  def serveArtifact(rai: RemoteApiInfo, fullUrl: URL): Box[LiftResponse] = Full(XmlResponse(
    <div>Artifact :{ fullUrl }</div>
    ))
  def serveArtifacts(artifactId: String): Box[LiftResponse] = Full(XmlResponse(
    <div>Artifacts :{ artifactId }</div>
    ))
}
