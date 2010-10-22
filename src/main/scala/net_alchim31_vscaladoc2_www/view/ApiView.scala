package net_alchim31_vscaladoc2_www.view

import java.net.URI
import net_alchim31_vscaladoc2_www.Helper4Laf
import net_alchim31_vscaladoc2_www.NavigatorDisplayer
import net_alchim31_vscaladoc2_www.NavigatorDisplayer4Laf
import net_alchim31_vscaladoc2_www.EntityDisplayer4Laf
import net_alchim31_vscaladoc2_www.EntityDisplayer4Debug
import net_alchim31_vscaladoc2_www.EntityDisplayer
import net_alchim31_vscaladoc2_www.BasicRawDataProvider
import net_alchim31_vscaladoc2_www.RawDataToInfo
import net_alchim31_vscaladoc2_www.UoaHelper
import net_alchim31_vscaladoc2_www.info._
import net_alchim31_vscaladoc2_www.model.Scaladoc
import net_alchim31_vscaladoc2_www.model.Scaladoc2
import net_alchim31_vscaladoc2_www.model.VScaladoc2
import net_alchim31_vscaladoc2_www.model.RemoteApiInfo
import java.util.Date
import java.net.URL
import net.liftweb.common._
import net.liftweb.util.Helpers.tryo
import _root_.net.liftweb.http._

//TODO optimization : set expiration to never, use a cache, etag,... in front of request as content from a url should be immutable (for non SNAPSHOT version)
//TODO manage special version : latest, latest-snapshot
object ApiView {

  private val _uoaHelper = new UoaHelper()
  private val _lafHelper = new  Helper4Laf(new URI(S.contextPath+ "/"), _uoaHelper)
  private val _rdti = new RawDataToInfo(new BasicRawDataProvider(), _uoaHelper)
  private val _entityDisplayer : EntityDisplayer = new EntityDisplayer4Laf(_lafHelper, _rdti)//new EntityDisplayer4Debug()
  private val _navigatorDisplayer : NavigatorDisplayer = new NavigatorDisplayer4Laf(_lafHelper, _rdti)//new EntityDisplayer4Debug()

  val dispatch: LiftRules.DispatchPF = {
    case Req("navigator" :: "api" :: artifactId :: version :: entityPath, _, GetRequest) => () => serveEntity(artifactId, version, entityPath)(serveNavigator)
    case Req("navigator" :: "_browser" :: "api" :: artifactId :: version :: Nil, _, GetRequest) => () => serveEntity(artifactId, version, Nil)(serveBrowser)
    case Req("navigator" :: "_rsrc" :: path, ext, GetRequest) => () => _navigatorDisplayer.serveResource(path, ext)
    case Req("raw" :: "api" :: artifactId :: version :: entityPath, _, GetRequest) => () => serveOriginal(artifactId, version, entityPath)
    case Req("laf" :: "api" :: artifactId :: version :: entityPath, _, GetRequest) => () => serveEntity(artifactId, version, entityPath)(serveApi)
    case Req("laf" :: "api" :: artifactId :: Nil, _, GetRequest) => () => _entityDisplayer.serveArtifacts(artifactId)
    case Req("laf" :: "_rsrc" :: path, ext, GetRequest) => () => _entityDisplayer.serveResource(path, ext)
    //case Req("api" :: "static" :: _, "json", GetRequest) => JString("Static")
  }


  def urlOf(v : RemoteApiInfo) = "navigator/api/" + v.artifactId + "/" + v.version

  //TODO should we transform entityPath to following ApiProvider way to create page ?
  private def serveOriginal(artifactId: String, version: String, entityPath: List[String]): Box[LiftResponse] = {
    for (
      api <- RemoteApiInfo.findApiOf(artifactId, version) ?~! ("no registered api for " + artifactId + "/" + version)
    ) yield {
      RedirectResponse(api.baseUrl.toExternalForm)
    }
  }

  private def serveEntity(artifactId : String, version : String, entityPath : List[String])(srv : (RemoteApiInfo, String, List[String]) => Box[LiftResponse]) : Box[LiftResponse] = {
      for (
        api <- RemoteApiInfo.findApiOf(artifactId, version) ?~! ("no registered api for " + artifactId + "/" + version) ;
        rurl <- api.provider.rurlPathOf(entityPath) ?~! ("no rules to find the url of " + entityPath);
        resp <- srv(api, rurl, entityPath)
      ) yield resp
  }

  private def serveApi(api: RemoteApiInfo, rurl: String, entityPath: List[String]): Box[LiftResponse] = {
    println("serveApi" + entityPath + " // "+ entityPath.length)
    api.provider match {
      case VScaladoc2 => _uoaHelper(api.artifactId.is :: api.version.is :: entityPath).flatMap(uoa =>serveApi(uoa))
      case _ => tryo(RedirectResponse(new URL(api.baseUrl, rurl).toExternalForm))
    }
  }

  private def serveApi(uoa : Uoa): Box[LiftResponse] = {
	  uoa match {
	    case uoa : Uoa4Artifact => _entityDisplayer.serveArtifact(uoa)
	    case uoa : Uoa4Package => _entityDisplayer.servePackage(uoa)
	    case uoa : Uoa4Type => _entityDisplayer.serveType(uoa)
	    case uoa : Uoa4Fieldext => _entityDisplayer.serveFieldext(uoa)
	  }
  }

  private def serveNavigator(api: RemoteApiInfo, rurl: String, entityPath: List[String]): Box[LiftResponse] = {
    api.provider match {
      case VScaladoc2 => _navigatorDisplayer.serveNavigator(api, entityPath)
      case _ => tryo(RedirectResponse(api.baseUrl.toExternalForm))
    }
  }

  private def serveBrowser(api: RemoteApiInfo, rurl: String, entityPath: List[String]): Box[LiftResponse] = {
    api.provider match {
      case VScaladoc2 => _navigatorDisplayer.serveBrowser(api, entityPath)
      case _ => Full(NotImplementedResponse())
    }
  }


}
