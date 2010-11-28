package net_alchim31_vscaladoc2_www

import java.util.Date
import java.util.TimeZone
import java.text.SimpleDateFormat

import net.liftweb.http.HeaderDefaults
import net.liftweb.util.Helpers
import scala.reflect.NameTransformer
import net.liftweb.http.S
import org.fusesource.scalate.Binding
import org.fusesource.scalate.RenderContext
import net_alchim31_vscaladoc2_www.model.RemoteApiInfo
import java.net.URI
import java.io.File
import org.fusesource.scalate.support.FileResourceLoader
import java.io.{ StringWriter, PrintWriter }
import java.net.URL
import net.liftweb.common.{ Full, Box }
import net.liftweb.http.{ XmlResponse, LiftResponse, InMemoryResponse, NotFoundResponse, NotImplementedResponse, StreamingResponse }
import org.fusesource.scalate.{ TemplateEngine, DefaultRenderContext }
import javax.activation.MimetypesFileTypeMap
import net_alchim31_vscaladoc2_www.info._

//TODO add production configuration (cache client, scalate working dir, ...)
class ScalateDisplayer(helper: Helper, tmplDir: File) {
  private lazy val httpDateFormat = {
    val b = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z")//, Locale.US)
    b.setTimeZone(TimeZone.getTimeZone("UTC"))
    b
  }
  private lazy val _mimetypesFileTypeMap = {
    val b = new MimetypesFileTypeMap()
    b.addMimeTypes("text/javascript   js")
    b.addMimeTypes("text/css          css")
    b
  }

  private val _engine = {
    val b = new TemplateEngine(List(tmplDir))
    b.bindings = List(
      Binding("helper", classOf[info.Helper].getName, true))

    //b.resourceLoader = new FileResourceLoader(Some(new File("/home/dwayne/work/oss/vscaladoc2_wwww/src/main/look1")))
    //engine.workingDirectory = new File("/var/lib/myapp/work")
    b.allowReload = true
    b.allowCaching = true
    b
  }

  private case class HtmlTextResponse(text: String, code: Int) extends LiftResponse with HeaderDefaults {
    def toResponse = {
      val bytes = text.getBytes("UTF-8")
      InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/html; charset=utf-8") :: headers, cookies, code)
    }
  }

  protected def renderHtml(templatePath: String)(fillCtx: (RenderContext) => Box[RenderContext]): Box[LiftResponse] = {
    val template = _engine.load(templatePath)
    val buffer = new StringWriter()
    val context = new DefaultRenderContext(templatePath, _engine, new PrintWriter(buffer))
    context.attributes("ping") = "pong"
    context.attributes("helper") = helper
    //context.attributes("info") = info
    for (context <- fillCtx(context)) yield {
      template.render(context)
      HtmlTextResponse(buffer.toString, 200)
    }
  }

  def serveResource(path: List[String], ext: String): Box[LiftResponse] = {
    val rsrcLoader = _engine.resourceLoader
    val rpath = path.mkString("/", "/", ".") + ext
    //println("try serve : " + rpath)
    rsrcLoader.resource(rpath) match {
      case None => Full(NotFoundResponse(""))
      case Some(rsrc) => {
        //println("serve : " + rsrc)
        rsrc.toFile match {
          case None => Full(StreamingResponse(rsrc.inputStream, () => {}, -1, Nil, Nil, 200))
          case Some(f) => {
            val mimeType = _mimetypesFileTypeMap.getContentType(f.getName)
            //println("mimetypes :" + mimeType + " // "+ f.getName)
            val size = f.length
            val httpHeaders = List(
                ("Content-Length", size.toString),
                ("Content-Type", mimeType),
                ("Last-Modified", httpDateFormat.format(new Date(f.lastModified)))
            )
            Full(StreamingResponse(rsrc.inputStream, () => {}, size, httpHeaders, Nil, 200))
          }
        }
      }
    }
  }

}

class Helper4Laf(baseUrl: URI, uoaHelper: UoaHelper, idp : InfoDataProvider) extends Helper {
  def urlOf(vs: String*): String = urlOf(vs.filter(_ != "").mkString("/"))
  def urlOf(v: String): String = baseUrl.resolve(v).toASCIIString
  def urlOf(uoa: Uoa, subContext: String = ""): String = urlOf(subContext, refPathOf(uoa))
  def refPathOf(uoa: Uoa): String = uoaHelper.toRefPath(uoa)

  def labelOf(v: String): String = uoaHelper(v) match {
    case Full(uoa) => labelOf(uoa)
    case _ => "labelOf(" + v + ")"
  }

  def labelOf(uoa: Uoa): String = uoa match {
    case Uoa4Artifact(artifactId, version) => artifactId + "-" + version
    case Uoa4Package(packageName, uoaArtifact) => packageName
    case Uoa4Type(typeName, uoaPackage) => NameTransformer.decode(typeName)//.split('.').reverse.head
    case Uoa4Fieldext(fieldextName, uoaType) => fieldextName
  }

  def fqNameOf(v: Uoa): String = v match {
    case Uoa4Artifact(artifactId, version) => artifactId + "-" + version
    case Uoa4Package(packageName, uoaArtifact) => if (packageName == "_root_") "" else packageName
    case Uoa4Type(typeName, uoaPackage) => fqNameOf(uoaPackage) + "." + NameTransformer.decode(typeName)
    case Uoa4Fieldext(fieldextName, uoaType) => fqNameOf(uoaType) + "." + fieldextName
  }
  
  def toArtifactInfo(uoa: Uoa4Artifact): Box[ArtifactInfo] = idp.toArtifactInfo(uoa)
  def toTypeInfo(uoa: Uoa4Type): List[Box[TypeInfo]] = idp.toTypeInfo(uoa)
  def toFieldextInfo(uoa: Uoa4Fieldext): List[Box[FieldextInfo]] = idp.toFieldextInfo(uoa)
}

class NavigatorDisplayer4Laf(helper: Helper, val rdti: InfoDataProvider, tmplDir: File) extends ScalateDisplayer(helper, tmplDir) with NavigatorDisplayer {
  def serveNavigator(rai: RemoteApiInfo, entityPath: List[String]): Box[LiftResponse] = renderHtml("/index.scaml") { context =>
    for ( artifact <- rdti.toArtifactInfo(Uoa4Artifact(rai.artifactId, rai.version))) yield {
      context.attributes("artifact") =  artifact
      context.attributes("entityPath") = entityPath
      context
    }
  }
  def serveBrowser(rai: RemoteApiInfo, entityPath: List[String]): Box[LiftResponse] = renderHtml("/browser.scaml") { context =>
    for ( artifact <- rdti.toArtifactInfo(Uoa4Artifact(rai.artifactId, rai.version))) yield {
      context.attributes("artifact") = artifact
      context.attributes("entityPath") = entityPath
      context.attributes("uoa4types") = {
        var b : List[Uoa4Type] = for(buoa <- rdti.findAllTypes(artifact.uoa); uoa <- buoa) yield uoa //Nil.asInstanceOf[List[Uoa4Type]]
        if (rai.artifactId.endsWith("_demoprj")) {
          b = b ++ (for (i <- 0 until 3000) yield { Uoa4Type("ZFakeEntry_" + i, Uoa4Package("fake.package_" + i % 3, artifact.uoa)) })
        }
        b
      }
      context
    }
  }

}

class EntityDisplayer4Laf(helper: Helper, val rdti: InfoDataProvider, tmplDir: File) extends ScalateDisplayer(helper, tmplDir) with EntityDisplayer {
  def serveArtifacts(artifactId: String): Box[LiftResponse] = Full(NotImplementedResponse())

  def serveFieldext(uoa: Uoa4Fieldext): Box[LiftResponse] = Full(NotImplementedResponse())

  def serveType(uoa: Uoa4Type): Box[LiftResponse] = renderHtml("/type.scaml") { context =>
    //TODO keep original failure from a List[Box[x]]
    rdti.toArtifactInfo(uoa.uoaPackage.uoaArtifact).map { a =>
      context.attributes("logo") = a.logo
      context.attributes("tpes") = rdti.toTypeInfo(uoa).map(_.open_!).sortWith((a, b) => a.kind == "object" || (a.kind < b.kind))
      context
    }
  }

  def servePackage(uoa: Uoa4Package): Box[LiftResponse] = renderHtml("/package.scaml") { context =>
    //TODO keep original failure from a List[Box[x]]
    rdti.toArtifactInfo(uoa.uoaArtifact).map { a =>
      context.attributes("logo") = a.logo
      context.attributes("pkgs") = rdti.toPackageInfo(uoa).map(_.open_!)
      context
    }
  }

  def serveArtifact(uoa: Uoa4Artifact): Box[LiftResponse] = renderHtml("/artifact.scaml") { context =>
    rdti.toArtifactInfo(uoa).map { a =>
      context.attributes("artifact") = a
      context
    }
  }

}

import net_alchim31_utils.{ FileSystemHelper, ClasspathHelper }
class LafProvider(cacheDir: File, helper: Helper, rdti: InfoDataProvider, fsh : FileSystemHelper) {

  private val _ch = new ClasspathHelper()

  private def extractLafIfNotExist(lafName: String): Box[File] = {
    val dir = new File(cacheDir, "laf/" + lafName)
    dir.exists match {
      case true => Full(dir)
      case false => {
        // TODO retrieve archive from DataBase instead of classpath
        println("laf : try to unarchive laf-" + lafName + ".jar into " + dir)
        _ch.findCPResourceAsStream("laf-" + lafName + ".jar").flatMap { is =>
          Helpers.tryo {
            fsh.unjar(dir, is)
            dir
          }
        }
      }
    }
  }

  def newEntityDisplayer(lafName: String = "navigator0"): Box[EntityDisplayer] = {
    extractLafIfNotExist(lafName).map(dir => new EntityDisplayer4Laf(helper, rdti, dir))
  }

  def newNavigatorDisplayer(lafName: String = "entity0"): Box[NavigatorDisplayer] = {
    extractLafIfNotExist(lafName).map(dir => new NavigatorDisplayer4Laf(helper, rdti, dir))
  }
}
