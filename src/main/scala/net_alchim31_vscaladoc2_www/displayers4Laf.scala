package net_alchim31_vscaladoc2_www

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

class ScalateDisplayer(helper : Helper, tmplDir: File) {
  private lazy val _mimetypesFileTypeMap = {
	val b = new MimetypesFileTypeMap()
	//b.addMimeTypes("text/javascript   js")
	b
  }

  private val _engine = {
    val b = new TemplateEngine(List(tmplDir))
    b.bindings = List(
      Binding("helper", classOf[info.Helper].getName, true)
    )

    //b.resourceLoader = new FileResourceLoader(Some(new File("/home/dwayne/work/oss/vscaladoc2_wwww/src/main/look1")))
    //engine.workingDirectory = new File("/var/lib/myapp/work")
    b.allowReload =  true
    b.allowCaching =  true
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
    val context = new DefaultRenderContext(templatePath, _engine, new PrintWriter(buffer))
    context.attributes("ping") = "pong"
    context.attributes("helper") = helper
    //context.attributes("info") = info
    fillCtx(context)
    template.render(context)
    Full(HtmlTextResponse(buffer.toString, Nil, 200))
  }

  def serveResource(path: List[String], ext : String): Box[LiftResponse] = {
    val rsrcLoader = _engine.resourceLoader
    val rpath = path.mkString("/", "/", ".") + ext
   	println("try serve : "  + rpath)
    rsrcLoader.resource(rpath) match {
      case None => Full(NotFoundResponse(""))
      case Some(rsrc) => {
    	println("serve : "  + rsrc)
        rsrc.toFile match {
          case None => Full(StreamingResponse(rsrc.inputStream, () => {}, -1, Nil, Nil, 200))
          case Some(f) => {
            val mimeType = _mimetypesFileTypeMap.getContentType(f)
            println("mimetypes :" + mimeType)
            val size = f.length
            Full(StreamingResponse(rsrc.inputStream, () => {}, size, List(("Content-Length", size.toString), ("Content-Type", mimeType)), Nil, 200))
          }
        }
      }
    }
  }

}

class Helper4Laf(baseUrl: URI, uoaHelper : UoaHelper) extends Helper {
  def urlOf(vs : String*) : String = urlOf(vs.filter(_ != "").mkString("/"))
  def urlOf(v: String): String = baseUrl.resolve(v).toASCIIString
  def urlOf(uoa : Uoa, subContext : String = "") : String = urlOf(subContext, uoaHelper.toRefPath(uoa))

  def labelOf(v: String): String = uoaHelper(v) match {
	case Full(uoa) => labelOf(uoa)
	case _ => "labelOf(" + v + ")"
  }

  def labelOf(uoa : Uoa): String = uoa match {
    case Uoa4Artifact(artifactId, version) => artifactId + "-" + version
    case Uoa4Package(packageName, uoaArtifact) => packageName
    case Uoa4Type(typeName, uoaPackage) => NameTransformer.decode(typeName).split('.').reverse.head
    case Uoa4Fieldext(fieldextName, uoaType) => fieldextName
  }

  def fqNameOf(v : Uoa) : String = v match {
    case Uoa4Artifact(artifactId, version) => artifactId + "-" + version
    case Uoa4Package(packageName, uoaArtifact) => if (packageName == "_root_") "" else packageName
    case Uoa4Type(typeName, uoaPackage) => fqNameOf(uoaPackage) + "." + NameTransformer.decode(typeName)
    case Uoa4Fieldext(fieldextName, uoaType) => fqNameOf(uoaType) + "." + fieldextName
  }
}

class NavigatorDisplayer4Laf(helper : Helper, val rdti : RawDataToInfo, tmplDir: File = new File("/home/dwayne/work/oss/vscaladoc2_www/src/main/laf/navigator0")) extends ScalateDisplayer(helper, tmplDir) with NavigatorDisplayer {
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
    context.attributes("uoa4types") =  rdti.findAllTypes(Uoa4Package("_root_", Uoa4Artifact(rai.artifactId, rai.version))).map(_.open_!)//Nil.asInstanceOf[List[Uoa4Type]]
  }

}

class EntityDisplayer4Laf(helper : Helper, val rdti : RawDataToInfo, tmplDir: File = new File("/home/dwayne/work/oss/vscaladoc2_www/src/main/laf/entity0")) extends ScalateDisplayer(helper, tmplDir) with EntityDisplayer {
  def serveArtifacts(artifactId: String): Box[LiftResponse] = Full(NotImplementedResponse())

  def servePackage(fullUrl: URL): Box[LiftResponse] = Full(NotImplementedResponse())
  def serveMember(fullUrl: URL): Box[LiftResponse] = Full(NotImplementedResponse())

  def serveType(rai: RemoteApiInfo, fullUrl: URL): Box[LiftResponse] = renderHtml("/type.scaml") { context =>
    println("serveType : " + fullUrl)
    context.attributes("title") = "Title" //TODO
    context.attributes("copyright") = "" //TODO
    val tpe = new TypeInfo() {
    def simpleName: String = "MyType"
    def signature: String = "signature"
    def description: HtmlString = "<p>Il fait beau </p>"
    def docTags: Seq[DocTag] = Nil
    def source: Option[URI] = None
    def uoa: Uoa4Type = Uoa4Type("MyType", Uoa4Package("my.package", Uoa4Artifact("myArtifact", "x.y.z")))
    def kind: String = "object"
    def isInherited(m: FieldextInfo) = m.uoa.uoaType == uoa
    def constructors: List[FieldextInfo] = Nil
    def fields: List[FieldextInfo] = Nil
    def methods: List[FieldextInfo] = Nil

    }
    //context.attributes("tpe") = tpe
    context.attributes("tpes") = List(tpe).sortWith((a, b) => a.kind == "object" || (a.kind < b.kind))
  }

  def serveArtifact(rai: RemoteApiInfo, fullUrl: URL): Box[LiftResponse] = renderHtml("/artifact.scaml") { context =>
    context.attributes("artifact") = new ArtifactInfo() {
      override def artifactId: String = rai.artifactId
      override def version: String = rai.version
      override def apiUrl = Some(rai.baseUrl.toURI)
    }
  }

}
