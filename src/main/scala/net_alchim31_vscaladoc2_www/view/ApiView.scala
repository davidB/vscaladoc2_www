package net_alchim31_vscaladoc2_www.view

import net_alchim31_vscaladoc2_www.NavigatorDisplayer
import net_alchim31_vscaladoc2_www.NavigatorDisplayer4Laf
import net_alchim31_vscaladoc2_www.EntityDisplayer4Laf
import net_alchim31_vscaladoc2_www.EntityDisplayer4Debug
import net_alchim31_vscaladoc2_www.EntityDisplayer
import net_alchim31_vscaladoc2_www.model.Scaladoc
import net_alchim31_vscaladoc2_www.model.Made
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

  private val _remoteApiInfos: List[RemoteApiInfo] = List[RemoteApiInfo](
    RemoteApiInfo("sample", "1.0.0", new URL("file://tmp/sample-api"), VScaladoc2, Made("0", new Date())),
    RemoteApiInfo("scala-library", "2.8.0", new URL("http://www.scala-lang.org/api/2.8.0/index.html"), Scaladoc2, Made("0", new Date())),
    RemoteApiInfo("scala-library", "2.7.7", new URL("http://www.scala-lang.org/api/2.7.7/index.html"), Scaladoc, Made("0", new Date()))
  )

  private val _entityDisplayer : EntityDisplayer = new EntityDisplayer4Laf()//new EntityDisplayer4Debug()
  private val _navigatorDisplayer : NavigatorDisplayer = new NavigatorDisplayer4Laf()//new EntityDisplayer4Debug()
  
  val dispatch: LiftRules.DispatchPF = {
    case Req("navigator" :: "api" :: artifactId :: version :: entityPath, _, GetRequest) => () => serveEntity(artifactId, version, entityPath)(serveNavigator)
    case Req("navigator" :: "_browser" :: "api" :: artifactId :: version :: Nil, _, GetRequest) => () => serveEntity(artifactId, version, Nil)(serveBrowser)
    case Req("navigator" :: "_rsrc" :: path, _, GetRequest) => () => _navigatorDisplayer.serveResource(path)
    case Req("raw" :: "api" :: artifactId :: version :: entityPath, _, GetRequest) => () => serveOriginal(artifactId, version, entityPath)
    case Req("laf" :: "_rsrc" :: path, _, GetRequest) => () => _entityDisplayer.serveResource(path)
    case Req("laf" :: "api" :: artifactId :: Nil, "html", GetRequest) => () => _entityDisplayer.serveArtifacts(artifactId)
    case Req("laf" :: "api" :: artifactId :: version :: entityPath, "html", GetRequest) => () => serveEntity(artifactId, version, entityPath)(serveApi)
    //case Req("api" :: "static" :: _, "json", GetRequest) => JString("Static")
  }


  //TODO should we transform entityPath to following ApiProvider way to create page ?
  def serveOriginal(artifactId: String, version: String, entityPath: List[String]): Box[LiftResponse] = {
    for (
      api <- findApiOf(artifactId, version) // ?~ "no registered api for " + artifactId + "/" + version
    ) yield {
      RedirectResponse(api.baseUrl.toExternalForm)
    }
  }

  private def serveEntity(artifactId : String, version : String, entityPath : List[String])(srv : (RemoteApiInfo, String, List[String]) => Box[LiftResponse]) : Box[LiftResponse] = {
      for (
        api <- findApiOf(artifactId, version); // ?~ "no registered api for " + artifactId + "/" + version ;
        rurl <- api.provider.rurlPathOf(entityPath);
        resp <- srv(api, rurl, entityPath)
      ) yield resp
  }
  
  private def serveApi(api: RemoteApiInfo, rurl: String, entityPath: List[String]): Box[LiftResponse] = {
    val fullUrl = new URL(api.baseUrl, rurl)
    api.provider match {
      case VScaladoc2 => entityPath.length match {
        case 0 => _entityDisplayer.serveArtifact(api, fullUrl)
        case 1 => _entityDisplayer.servePackage(fullUrl)
        case 2 => _entityDisplayer.serveType(fullUrl)
        case 3 | 4 => _entityDisplayer.serveMember(fullUrl)
      }
      case _ => tryo(RedirectResponse(fullUrl.toExternalForm))
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

  //TODO sort remoteApiInfos by version / ApiProvider
  //TODO support special version (latest, ...)
  private def findApiOf(artifactId: String, version: String): Box[RemoteApiInfo] = {
    _remoteApiInfos.find(x => x.artifactId == artifactId && x.version == version) match {
      case Some(api) => Full(api)
      case None => println("f0") ; Failure("api for " + artifactId + "::" + version + " is not registered")
    }
  }
}
