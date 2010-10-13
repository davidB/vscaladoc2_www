package net_alchim31_vscaladoc2_www.snippet

import net.liftweb.mapper.view.MapperPaginatorSnippet
import net.liftweb.http.DispatchSnippet
import net_alchim31_vscaladoc2_www.view.ApiView
import net_alchim31_vscaladoc2_www.model.RemoteApiInfo
import scala.xml.NodeSeq
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.mapper.{OrderBy, MaxRows, Ascending, Descending}

class Listings extends DispatchSnippet {
  override def dispatch = {
    case "all" => all _
    case "top" => top _
    case "paginate" => paginator.paginate _
  }
  val paginator = new MapperPaginatorSnippet(RemoteApiInfo) {
    override def itemsPerPage = 5
    constantParams = OrderBy(RemoteApiInfo.artifactId, Ascending) :: Nil
  }
  def all(xhtml: NodeSeq): NodeSeq = many(paginator.page, xhtml)
  def top(xhtml: NodeSeq) = many(RemoteApiInfo.findAll(MaxRows(3), OrderBy(RemoteApiInfo.id, Descending)), xhtml)

  protected def many(apis: List[RemoteApiInfo], xhtml: NodeSeq): NodeSeq = apis.flatMap(a => single(a, xhtml))

  protected def single(api: RemoteApiInfo, xhtml: NodeSeq): NodeSeq = {
    Helpers.bind("api", xhtml,
      "artifactId" -> api.artifactId,
      "version" -> api.version,
      "format" -> api.format,
      //"link" -%> <a href={ ApiView.urlOf(api) }>details >></a>
      AttrBindParam("url", ApiView.urlOf(api), "href")
    )
  }

}