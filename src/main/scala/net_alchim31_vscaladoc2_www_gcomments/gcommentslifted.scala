package net_alchim31_vscaladoc2_www_gcomments

import net.liftweb.common.Loggable
import net.liftweb.mapper.ByList
import net.liftweb.mapper.CRUDify
import net.liftweb.mapper.LongKeyedMetaMapper
import net.liftweb.sitemap.Loc.LocGroup
import net.liftweb.mapper.MappedDateTime
import net.liftweb.mapper.MappedInt
import net.liftweb.mapper.IdPK
import net.liftweb.mapper.MappedString
import net.liftweb.mapper.LongKeyedMapper
import net.liftweb.mapper.By
import java.net.URI
import net.liftweb.common.{ Box, Empty, Full, Failure }
import net.liftweb.util.Helpers.tryo
import org.joda.time.DateTime
import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.xml.{ NodeSeq, Elem, XML }
import net.liftweb.util.{ActorPing, TimeHelpers}
import net_alchim31_vscaladoc2_www.model.RemoteApiInfo
import net_alchim31_vscaladoc2_www.{CommentSystem, UoaHelper}

//TODO remove dependency to RemoteApiInfo object
class GCommentsService(urlMaker : UrlMaker4GComments, uoaHelper : UoaHelper) extends GCommentsInfoStore with CommentSystem with Loggable {
  
  def init() {
    ActorPing.schedule(this.updateInfoFromFeeds, TimeHelpers.seconds(10) ) 
  }
  
  def findByRefPaths(refPaths : Seq[String]) : Map[String, Info] = {
    import scala.collection.mutable
    
    val back = mutable.Map[String, Info]()
    if (refPaths.size > 0) {
      //only search for refPath with ggroupId
      val refPathsToSearch = new ListBuffer[String]()
      for (rp <- refPaths) {
        //retrieve ggroupId from refPath, because several ggroupId could be present in one requete (inherited could come from an other project) and then remove ggroupId from ArtifactInfo,...
        findGroupIdOf(rp) match {
          case Full(ggroupId) => {
            refPathsToSearch += rp
            back += ((rp, Info(0, urlMaker.postNew(ggroupId, rp))))
          }
          // Failure and Empty
          case _ => back += ((rp, Info(-1, null)))
        }
      }
      //WARN refPath from refPath include version, and version should be part of the returned key, but version are anonymized (encoded) in GComments
      val refPathMapping = refPathsToSearch.map(x => (urlMaker.transcode(x), x)).toMap
      val existing = GCommentsInfoMapped.findAll(ByList(GCommentsInfoMapped.refPath, refPathMapping.keys.toSeq))
      for (found <- existing) {
        back += ((refPathMapping(found.refPath.is), Info(found.nb.is, urlMaker.messages(found.ggroupId.is, found.threadId.is, found.firstId.is))))
      }
    }
    logger.debug("in : " + refPaths)
    logger.debug("return : " + back)
    back.toMap
  }
  
  def findGroupIdOf(refPath : String) : Box[String] = {
    //TODO add cache
    uoaHelper.toUoa4Artifact(refPath).flatMap{ uoa =>
      RemoteApiInfo.findApiOf(uoa.artifactId, uoa.version).map(_.ggroupId.is).filter(_.trim.length > 0)
    }
  }
  
  def findAllGGroupId() : List[String] = RemoteApiInfo.findAllGGroupId()
  
  def findByRefPath(refPath : String) : Box[GCommentsInfo] = {
    GCommentsInfoMapped.findAll(By(GCommentsInfoMapped.refPath, refPath)).headOption match {
      case Some(e) => Full(new GCommentsInfoView(e))
      case None => Empty
    }
  }
  
  def save(v : Iterable[GCommentsInfo]) : Int = {
    var b = 0
    for (e <- v) {
      e match {
        case view : GCommentsInfoView => view.v.save()
        case _ => {
          (GCommentsInfoMapped.create
              .refPath(e.refPath)
              .nb(e.nb)
              .firstAt(e.firstAt.toDate)
              .lastAt(e.lastAt.toDate)
              .ggroupId(e.ggroupId)
              .threadId(e.threadId)
              .firstId(e.firstId)
              ).save()
        }
      }
      b += 1
    }
    b
  }
  
  private lazy val _gcommentsGlobal = {
    val ggroupId = "__global__"
    GCommentsInfoMapped.findAll(By(GCommentsInfoMapped.ggroupId, ggroupId)).headOption.getOrElse{
      val b = (GCommentsInfoMapped.create
        .firstAt(new Date(0))
        .lastAt(new Date(0))
        .ggroupId(ggroupId)
      )
      b.save()
      b
    }
  }
  
  def lastBatchUpdate : DateTime = new DateTime(_gcommentsGlobal.lastAt.is)
  def lastBatchUpdate_=(v : DateTime) = _gcommentsGlobal.lastAt(v.toDate).save

  class GCommentsInfoView(val v : GCommentsInfoMapped) extends GCommentsInfo {
    def refPath : String = v.refPath.is
    def nb : Int = v.nb.is
    def firstAt : DateTime = new DateTime(v.firstAt.is)
    def lastAt : DateTime = new DateTime(v.lastAt.is)
    def ggroupId : String = v.ggroupId.is
    def threadId : String = v.threadId.is
    def firstId : String = v.firstId.is

    def merge(e : GCommentEntryInfo) : GCommentsInfo = {
      if (e.refPath != refPath || (e.at.isAfter(firstAt) && e.at.isBefore(lastAt))) {
        this
      } else {
        val newFirstAt = if (firstAt.isBefore(e.at)) firstAt else e.at
        val newLastAt = if (lastAt.isBefore(e.at)) e.at else lastAt
        val newFirstId = if (newFirstAt eq e.at) e.msgId else firstId
        if (v.ggroupId.is == v.ggroupId.defaultValue) {
          v.ggroupId(e.ggroupId).threadId(e.threadId)
        }
        v.refPath(refPath).nb(nb + 1).firstAt(newFirstAt.toDate).lastAt(newLastAt.toDate).firstId(newFirstId)
        this
      }
    }
  }

  def updateInfoFromFeeds() {
    try {
      logger.info("update comments info from feeds")
      val extractor = new GCommentsFeedExtractor(urlMaker)
      extractor.updateFromAtomFeeds(this)
      logger.info("update comments info from feeds ... DONE")
    } finally {
      ActorPing.schedule(this.updateInfoFromFeeds, TimeHelpers.minutes(30) ) //TODO read frequency from configuration 
    }
  }
}

object GCommentsInfoMapped extends GCommentsInfoMapped with LongKeyedMetaMapper[GCommentsInfoMapped] with CRUDify[Long, GCommentsInfoMapped] {
  override def dbTableName = "gcomments"
  override def fieldOrder = List(refPath, nb, firstAt, lastAt, ggroupId, threadId, firstId)

  // crudify
  override def pageWrapper(body : NodeSeq) = <lift:surround with="admin" at="content">{ body }</lift:surround>
  override def calcPrefix = List("admin", _dbTableNameLC)
  override def displayName = "GComments"
  override def showAllMenuLocParams = LocGroup("admin") :: Nil
  override def createMenuLocParams = LocGroup("admin") :: Nil
  override def viewMenuLocParams = LocGroup("admin") :: Nil
  override def editMenuLocParams = LocGroup("admin") :: Nil
  override def deleteMenuLocParams = LocGroup("admin") :: Nil

}

//TODO add constraints on column
class GCommentsInfoMapped extends LongKeyedMapper[GCommentsInfoMapped] with IdPK {
  def getSingleton = GCommentsInfoMapped

  // fields
  object refPath extends MappedString(this, 250) // TODO optimize for query
  object nb extends MappedInt(this)
  object firstAt extends MappedDateTime(this)
  object lastAt extends MappedDateTime(this)
  object ggroupId extends MappedString(this, 64)
  object threadId extends MappedString(this, 20)
  object firstId extends MappedString(this, 20)
}
