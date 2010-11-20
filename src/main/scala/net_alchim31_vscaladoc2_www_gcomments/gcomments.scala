package net_alchim31_vscaladoc2_www_gcomments
import net.liftweb.common.Loggable

import dispatch.Request
import dispatch.Http
import net_alchim31_vscaladoc2_www.UoaHelper
import java.net.URLEncoder
import java.net.URL
import java.net.URI
import net.liftweb.common.{Box, Empty, Full, Failure}
import net.liftweb.util.Helpers.tryo
import org.joda.time.DateTime
import scala.collection.mutable.ListBuffer
import scala.xml.{NodeSeq, Elem, XML}

trait GCommentsInfoStore {
  def findAllGGroupId() : List[String]
  def findByRefPath(refPath : String) : Box[GCommentsInfo]
  def save(v : Iterable[GCommentsInfo]) : Int
  def lastBatchUpdate : DateTime
  def lastBatchUpdate_=(v : DateTime)
}

trait GCommentsInfo {
  def refPath : String
  def nb : Int
  def firstAt : DateTime
  def lastAt : DateTime
  def ggroupId : String
  def threadId : String
  def firstId: String
  
  /**
   * if refPath of e != this.refPath and e.at already between firstAt, lastAt, then e is ignored (no exception)
   * 
   * @param es  a list sort by date _.at, else some data could be ignore
   * @param checkNoLost check that nb increase with size of es 
   * @return cumul of call to merge(GCommentEntryInfo)
   * @throws IllegalStateException if some entry was
   */
  def merge(es : List[GCommentEntryInfo], checkNoLost : Boolean) : GCommentsInfo = {
    val cumul = es.foldLeft(this.asInstanceOf[GCommentsInfo])((cumul, e) => cumul.merge(e))
    if (checkNoLost && ((es.length + this.nb)!= cumul.nb)) {
     throw new IllegalStateException("some entry was lost during cumul") 
    }
    cumul
  }    

  /**
   * if refPath of e != this.refPath and e.at already between firstAt, lastAt, then e is ignored (no exception)
   * 
   * @param e
   * @return new ( this + e if valid info)
   */
  def merge(e : GCommentEntryInfo) : GCommentsInfo
}

protected case class GCommentEntryInfo(refPath : String, at : DateTime, ggroupId : String, threadId : String, msgId: String)

//case class(uoa : Uoa, nbComments : Int, extUrl :URI)
protected case class GCommentsInfo0(refPath : String, nb : Int, firstAt : DateTime, lastAt : DateTime, ggroupId : String, threadId : String, firstId: String) extends GCommentsInfo {

  /**
   * if refPath of e != this.refPath and e.at already between firstAt, lastAt, then e is ignored (no exception)
   * 
   * @param e
   * @return new ( this + e if valid info)
   */
  def merge(e : GCommentEntryInfo) : GCommentsInfo = {
    if (e.refPath != refPath || (e.at.isAfter(firstAt) && e.at.isBefore(lastAt))) {
      this
    } else {
      val newFirstAt = if (firstAt.isBefore(e.at))  firstAt else e.at
      val newLastAt = if (lastAt.isBefore(e.at))  e.at else lastAt
      val newFirstId = if (newFirstAt eq e.at) e.msgId else firstId
      GCommentsInfo0(refPath, nb +1, newFirstAt, newLastAt, ggroupId, threadId, newFirstId)
    }
  }
}

class UrlMaker4GComments(uoaHelper : UoaHelper) {
  protected[net_alchim31_vscaladoc2_www_gcomments]
  val ApiTitle  = """\[API\]\[Discuss\] (\S*)""".r

  protected[net_alchim31_vscaladoc2_www_gcomments]
  val UrlMessage = """http://groups.google.com/group/([a-zA-Z_0-9]*)/browse_thread/thread/([a-zA-Z_0-9]*)/([a-zA-Z_0-9]*)\?show_docid=([a-zA-Z_0-9]*)""".r

  def transcode(refPath : String) = uoaHelper.anonymizeVersion(refPath)._2
  
  def feed(ggroupId : String) = new URI(String.format("http://groups.google.com/group/%s/feed/atom_v1_0_msgs.xml", ggroupId))
  
  def postNew(ggroupId : String, refPath : String) = {
    def encode(s :String) = URLEncoder.encode(s, "UTF-8").replace("+", "%20")
    
    // url http://groups.google.com/group/${ggroupId}/post doesnt work in a frame
    val (version, refPathNoVersion) = uoaHelper.anonymizeVersion(refPath)
    val subject = encode("[API][Discuss] " + refPathNoVersion)
    val body = encode("version : ["+ version +"](http://vscaladoc.alchim31.net/navigator/api/" + refPath + ")") //TODO remove hardcoded base Url
    new URI(String.format("mailto:%s@googlegroups.com?subject=%s&body=%s", ggroupId, subject, body))
  }
  def messages(ggroupId : String, threadId : String, msgId : String) = new URI(String.format("http://groups.google.com/group/%s/browse_thread/thread/%s#msg_%s", ggroupId, threadId, msgId))
}

// TODO ignore data already processed, check update time
class GCommentsFeedExtractor(val urlMaker : UrlMaker4GComments) extends Loggable {
  //TODO find a better pattern to check Subject about API
  
  //private val _dateParser = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  
  // TODO to persist
  private var _lastUpdateAt = new DateTime(0)


  protected[net_alchim31_vscaladoc2_www_gcomments]
  def fusionEntries(entries : Iterable[GCommentEntryInfo], find : String => Box[GCommentsInfo]) : Iterable[Box[GCommentsInfo]] = {
    logger.info("fusion Entries :" + entries.size)
    for ((k, v) <- entries.groupBy(_.refPath )) yield {
      val v2 = v.toList.sortWith((x1 , x2) => x1.at.isBefore(x2.at))
      find(k) match {
        case Empty => tryo {
          val head = v2.head
          GCommentsInfo0(head.refPath, 1, head.at, head.at, head.ggroupId, head.threadId, head.msgId).merge(v2.tail, true) 
        }
        case Full(start) => tryo {
          start.merge(v2, false)
        }
        case f : Failure => f
      }
    }
  }
  
  protected[net_alchim31_vscaladoc2_www_gcomments]
  def collectEntryInfoFromAtomFeed(xml : Elem, after : DateTime) : Iterable[GCommentEntryInfo] = {
    val updated = new DateTime((xml \ "updated").text)
    val collected = new ListBuffer[GCommentEntryInfo]()
    if (updated.isAfter(after)) {
      for (entry <- xml \ "entry") {// \\ "{http://www.w3.org/2005/Atom}entry")) {
        val entryAt = new DateTime((entry \ "updated").text)
        if (entryAt.isAfter(after)) {
          //Regex extractor doesn't work if there is string before (like Re:)
          for (m <- urlMaker.ApiTitle.findFirstMatchIn((entry \ "title").text)) {
            val refPath = m.group(1)
            val href = (entry \ "link" \ "@href").text
            href match {
              case urlMaker.UrlMessage(ggroupId, threadId, msgId, docId) => collected += GCommentEntryInfo(refPath, entryAt, ggroupId, threadId, msgId)
              case _ => () //ignore
            }
          }
        }
      }
      collected.toIterable
    } else {
      logger.info("no data to collect, feed updated at " + updated)
      Nil
    }
  }
  
  protected def updateData(xml : Elem, find : String => Box[GCommentsInfo], save : Iterable[GCommentsInfo] => Int, lastRunAt : DateTime) = {
    save(fusionEntries(collectEntryInfoFromAtomFeed(xml, lastRunAt), find).collect{ case Full(ci) => ci })
  }
  
  def updateFromAtomFeeds(store : GCommentsInfoStore) : Int = {
    // google forbid access for some or undefined user-agent 
    // => need to provided a user aggent
    // an other way to define user-agent
    // var request = new Request(url) 
    // val req_with_agent = request <:< Map("User-Agent" -> "Mozilla/4.0") 
    // val responseBody = Http (req_with_agent as_str)
    val now = new DateTime()
    val previous = store.lastBatchUpdate
    val httpClient = new Http()
    httpClient.client.getParams.setParameter("User-Agent", "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1)")
    val savedCnt = for (ggroupId <- store.findAllGGroupId()) yield {
      val url = urlMaker.feed(ggroupId).toURL.toString
      val xml = new Http()(new Request(url) >> {is => XML.load(is) })
      val nb = updateData(xml, store.findByRefPath, store.save, previous)
      logger.info("updates from " + url + " dated after " + previous + " : " + nb)
      nb
    }
    store.lastBatchUpdate = now
    savedCnt.foldLeft(0)( _ + _)
  }
}
