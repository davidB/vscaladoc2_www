package net_alchim31_vscaladoc2_www

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

class ScalateDisplayer(tmplDir: File) {
  private lazy val _mimetypesFileTypeMap = new MimetypesFileTypeMap()

  private val _engine = {
    val b = new TemplateEngine(Some(tmplDir))
    //b.resourceLoader = new FileResourceLoader(Some(new File("/home/dwayne/work/oss/vscaladoc2_wwww/src/main/look1")))
    //engine.bindings = List(Binding("helper", "MyHelper", true))
    //engine.workingDirectory = new File("/var/lib/myapp/work")  
    //engine.allowReload =  false
    //engine.allowCaching =  false
    b
  }

  private case class HtmlTextResponse(text: String, headers: List[(String, String)], code: Int) extends LiftResponse {
    def toResponse = {
      val bytes = text.getBytes("UTF-8")
      InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/html; charset=utf-8") :: headers, Nil, code)
    }
  }

  protected def renderHtml(templatePath: String)(fillCtx: (RenderContext) => Unit): Box[LiftResponse] = {
    val template = _engine.load(templatePath)
    val buffer = new StringWriter()
    val context = new DefaultRenderContext(_engine, new PrintWriter(buffer))
    //context.attributes += "helper" -> new MyHelper    
    fillCtx(context)
    template.render(context)
    Full(HtmlTextResponse(buffer.toString, Nil, 200))
  }

  def serveResource(path: List[String]): Box[LiftResponse] = {
    val rsrcLoader = _engine.resourceLoader
    rsrcLoader.resource(path.mkString("/", "/", "")) match {
      case None => Full(NotFoundResponse(""))
      case Some(rsrc) => {
        rsrc.toFile match {
          case None => Full(StreamingResponse(rsrc.inputStream, () => {}, -1, Nil, Nil, 200))
          case Some(f) => {
            val mimeType = _mimetypesFileTypeMap.getContentType(f)
            val size = f.length
            Full(StreamingResponse(rsrc.inputStream, () => {}, size, List(("Content-Length", size.toString), ("Content-Type", mimeType)), Nil, 200))
          }
        }
      }
    }
  }

}

class Helper4Laf(baseUrl: URI) extends info.Helper {
  def urlOf(v: String): String = baseUrl.resolve(v).toASCIIString
  def urlOf(v: URI): String = baseUrl.resolve(v).toASCIIString
  def labelOf(v: String): String = "labelOf(" + v + ")"
  def labelOf(v: URI): String = "labelOf(" + v + ")" //TODO use the last fragment of the URI except for method/signature64
}

class NavigatorDisplayer4Laf(tmplDir: File = new File("/home/dwayne/work/oss/vscaladoc2_www/src/main/laf/navigator0")) extends ScalateDisplayer(tmplDir) with NavigatorDisplayer {
  import info._
  def serveNavigator(rai: RemoteApiInfo, entityPath: List[String]): Box[LiftResponse] = renderHtml("/index.scaml") { context =>
    context.attributes("artifact") = new ArtifactInfo() {
      override def artifactId: String = rai.artifactId
      override def version: String = rai.version
      override def apiUrl = Some(rai.baseUrl.toURI)
    }
    context.attributes("entityPath") = entityPath
  }
  def serveBrowser(rai: RemoteApiInfo, entityPath: List[String]): Box[LiftResponse] = renderHtml("/browser.scaml") { context =>
    context.attributes("artifact") = new ArtifactInfo() {
      override def artifactId: String = rai.artifactId
      override def version: String = rai.version
      override def apiUrl = Some(rai.baseUrl.toURI)
    }
    context.attributes("entityPath") = entityPath
  }

}

class EntityDisplayer4Laf(tmplDir: File = new File("/home/dwayne/work/oss/vscaladoc2_www/src/main/laf/entity0")) extends ScalateDisplayer(tmplDir) with EntityDisplayer {
  import info._

  def serveMember(fullUrl: URL): Box[LiftResponse] = Full(NotImplementedResponse())
  def serveType(fullUrl: URL): Box[LiftResponse] = Full(NotImplementedResponse())
  def servePackage(fullUrl: URL): Box[LiftResponse] = Full(NotImplementedResponse())

  def serveArtifact(rai: RemoteApiInfo, fullUrl: URL): Box[LiftResponse] = renderHtml("/artifact.scaml") { context =>
    context.attributes("artifact") = new ArtifactInfo() {
      override def artifactId: String = rai.artifactId
      override def version: String = rai.version
      override def apiUrl = Some(rai.baseUrl.toURI)
    }
  }

  def serveArtifacts(artifactId: String): Box[LiftResponse] = Full(NotImplementedResponse())
}
