package net_alchim31_vscaladoc2_www.view

import java.net.URL
import java.io.FileNotFoundException
import java.io.File
import java.net.URI
import java.util.Date
import _root_.net.liftweb.common._
import _root_.net.liftweb.util.Helpers.tryo
import _root_.net.liftweb.http._
import net_alchim31_vscaladoc2_www.NavigatorDisplayer
import net_alchim31_vscaladoc2_www.EntityDisplayer
import net_alchim31_vscaladoc2_www.ApiService
import net_alchim31_vscaladoc2_www.UoaHelper
import net_alchim31_vscaladoc2_www.AppServices
import net_alchim31_vscaladoc2_www.info._
import net_alchim31_vscaladoc2_www.model.Scaladoc
import net_alchim31_vscaladoc2_www.model.Scaladoc2
import net_alchim31_vscaladoc2_www.model.VScaladoc2
import net_alchim31_vscaladoc2_www.model.RemoteApiInfo

//TODO optimization : set expiration to never, use a cache, etag,... in front of request as content from a url should be immutable (for non SNAPSHOT version)
//TODO use guice to manage construction (injection via setter to avoid circular dependencies)
//TODO manage special version : latest, latest-snapshot
object ApiView extends Loggable {
  lazy val _uoaHelper = AppServices.uoaHelper
  lazy val _entityDisplayer: Box[EntityDisplayer] = AppServices.entityDisplayer
  lazy val _navigatorDisplayer: Box[NavigatorDisplayer] = AppServices.navigatorDisplayer
  
  val dispatch: LiftRules.DispatchPF = {
    case Req("navigator" :: "api" :: artifactId :: version :: entityPath, _, GetRequest) => () => failureConverter(serveEntity(artifactId, version, entityPath)(serveNavigator))
    case Req("navigator" :: "_browser" :: "api" :: artifactId :: version :: Nil, _, GetRequest) => () => failureConverter(serveEntity(artifactId, version, Nil)(serveBrowser))
    case Req("navigator" :: "_rsrc" :: path, ext, GetRequest) => () => failureConverter(_navigatorDisplayer.flatMap(_.serveResource(path, ext)))
    case Req("raw" :: "api" :: artifactId :: version :: entityPath, _, GetRequest) => () => failureConverter(serveOriginal(artifactId, version, entityPath))
    case Req("laf" :: "api" :: artifactId :: version :: entityPath, _, GetRequest) => () => failureConverter(serveEntity(artifactId, version, entityPath)(serveApi))
    case Req("laf" :: "api" :: artifactId :: Nil, _, GetRequest) => () => failureConverter(_entityDisplayer.flatMap(_.serveArtifacts(artifactId)))
    case Req("laf" :: "_rsrc" :: path, ext, GetRequest) => () => failureConverter(_entityDisplayer.flatMap(_.serveResource(path, ext)))
    //case Req("api" :: "static" :: _, "json", GetRequest) => JString("Static")
  }

  def urlOf(v: RemoteApiInfo) = "navigator/api/" + v.artifactId + "/" + v.version

  //TODO should we transform entityPath to following ApiProvider way to create page ?
  private def serveOriginal(artifactId: String, version: String, entityPath: List[String]): Box[LiftResponse] = {
    for (
      api <- RemoteApiInfo.findApiOf(artifactId, version) ?~! ("no registered api for " + artifactId + "/" + version)
    ) yield {
      RedirectResponse(api.baseUrl.toASCIIString)
    }
  }

  private def serveEntity(artifactId: String, version: String, entityPath: List[String])(srv: (RemoteApiInfo, String, List[String]) => Box[LiftResponse]): Box[LiftResponse] = {
    for (
      api <- RemoteApiInfo.findApiOf(artifactId, version) ?~! ("no registered api for " + artifactId + "/" + version);
      rurl <- api.provider.rurlPathOf(entityPath) ?~! ("no rules to find the url of " + entityPath);
      resp <- srv(api, rurl, entityPath)
    ) yield resp
  }

  private def serveApi(api: RemoteApiInfo, rurl: String, entityPath: List[String]): Box[LiftResponse] = {
    api.provider match {
      case VScaladoc2 => _uoaHelper(api.artifactId.is :: api.version.is :: entityPath).flatMap(uoa => serveApi(uoa))
      case _ => tryo(RedirectResponse(new URL(api.baseUrl + rurl).toExternalForm))
    }
  }

  private def serveApi(uoa: Uoa): Box[LiftResponse] = {
    _entityDisplayer.flatMap { entityDisplayer =>
      uoa match {
        case uoa: Uoa4Artifact => entityDisplayer.serveArtifact(uoa)
        case uoa: Uoa4Package => entityDisplayer.servePackage(uoa)
        case uoa: Uoa4Type => entityDisplayer.serveType(uoa)
        case uoa: Uoa4Fieldext => entityDisplayer.serveFieldext(uoa)
      }
    }
  }

  private def serveNavigator(api: RemoteApiInfo, rurl: String, entityPath: List[String]): Box[LiftResponse] = {
    api.provider match {
      case VScaladoc2 => _navigatorDisplayer.flatMap(_.serveNavigator(api, entityPath))
      case _ => tryo(RedirectResponse(api.baseUrl.toASCIIString))
    }
  }

  private def serveBrowser(api: RemoteApiInfo, rurl: String, entityPath: List[String]): Box[LiftResponse] = {
    api.provider match {
      case VScaladoc2 => _navigatorDisplayer.flatMap(_.serveBrowser(api, entityPath))
      case _ => Full(NotImplementedResponse())
    }
  }

  private def failureConverter(v: Box[LiftResponse]): Box[LiftResponse] = {
    v match {
      case f: Failure => {
        logger.warn(f)
        f.exception match {
          case Full(e: FileNotFoundException) => Full(NotFoundResponse("please contact admin"))
          case _ => v
        }
      }
      case _ => v
    }
  }
}
