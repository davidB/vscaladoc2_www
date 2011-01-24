package net_alchim31_vscaladoc2_www.snippet

import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.RequestVar
import net.liftweb.mapper.view.MapperPaginatorSnippet
import net.liftweb.http.DispatchSnippet
import net_alchim31_vscaladoc2_www.view.ApiView
import net_alchim31_vscaladoc2_www.model.RemoteApiInfo
import scala.xml.NodeSeq
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.mapper.{OrderBy, OrderBySql, MaxRows, Ascending, Descending, IHaveValidatedThisSQL, By, NotBy, BySql, QueryParam}
import scala.xml.Node
import net.liftweb.common.Box

class Listings extends DispatchSnippet {
  override def dispatch = {
    case "all" => all _
    case "top" => top _
    case "paginate" => paginator.paginate _
  }
  
  val defaultQueryParams : Seq[QueryParam[RemoteApiInfo]] = BySql[RemoteApiInfo]("parent IS NULL", IHaveValidatedThisSQL("davidB", "2011-10-21")) :: Nil
  
  val paginator = new MapperPaginatorSnippet(RemoteApiInfo) {
    override def itemsPerPage = 50
    constantParams =  defaultQueryParams //OrderBySql[RemoteApiInfo]("id  DESC GROUP BY id", IHaveValidatedThisSQL("me", "2010-11-06") ) :: Nil
  }
  def all(xhtml: NodeSeq): NodeSeq = {
    val data = RemoteApiInfo.findAll(defaultQueryParams : _*)
    val datags = data.groupBy(_.artifactId.is).toList.sortWith(_._1 < _._1)
    manyg(datags, xhtml)
  }
  def top(xhtml: NodeSeq) = many(RemoteApiInfo.findAll((defaultQueryParams :+ MaxRows(3)) : _*), xhtml) // NotBy(RemoteApiInfo.available, false)

  protected def many(apis: List[RemoteApiInfo], xhtml: NodeSeq): NodeSeq = apis.flatMap(a => single(a, xhtml))
  protected def manyg(apis: List[(String, Seq[RemoteApiInfo])], xhtml: NodeSeq): NodeSeq = apis.flatMap(a => group(a, xhtml))

  protected def group(apis: (String, Seq[RemoteApiInfo]), xhtml: NodeSeq): NodeSeq = {
    val versions : Box[NodeSeq] = Helpers.findNode(<api:versions/>, xhtml).map { tmpl =>
      apis._2.sortWith(_.version.is > _.version.is).map{ api =>
        Helpers.bind("api", tmpl.child,
          "version" -> api.version,
          "format" -> api.format,
          //"link" -%> <a href={ ApiView.urlOf(api) }>details >></a>
          AttrBindParam("url", ApiView.urlOf(api), "href")
        )
      }.flatten
    }
    println("versions", versions)
    Helpers.bind("api", xhtml,
      "artifactId" -> apis._1,
      BoxBindParam("versions", versions)
    )
  }

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