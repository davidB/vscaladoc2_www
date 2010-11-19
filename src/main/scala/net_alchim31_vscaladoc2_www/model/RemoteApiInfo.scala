package net_alchim31_vscaladoc2_www.model


import net.liftweb.mapper.{IHaveValidatedThisSQL, BySql}
import java.util.regex.Pattern
import java.net.URI
import net.liftweb.mapper.MappedEnum
import net.liftweb.mapper.Mapper
import net.liftweb.mapper.By
import net.liftweb.mapper.OrderBy
import net.liftweb.mapper.LongMappedMapper
import net.liftweb.mapper.MappedDateTime
import net.liftweb.mapper.MappedString
import net.liftweb.mapper.MappedString
import net.liftweb.sitemap.Loc.LocGroup
import scala.xml.NodeSeq
import net.liftweb.mapper.CRUDify
import net.liftweb.mapper.LongKeyedMetaMapper
import net.liftweb.mapper.IdPK
import net.liftweb.mapper.CreatedUpdated
import net.liftweb.mapper.LongKeyedMapper
import net.liftweb.common.{ Box, Full, Empty, Failure }
import java.net.URL
import java.util.Date
import _root_.net.liftweb.util.Helpers._

//case class Made(by: String, at: Date)
//case class RemoteApiInfo(artifactId: String, version: String, baseUrl: URL, provider: ApiProvider, made: Made)

//  import net.liftweb.common.{Full,Box,Empty,Failure}
//  import net.liftweb.mapper._
//  import net.liftweb.sitemap.Loc._
//  import scala.xml.NodeSeq

object RemoteApiInfo extends RemoteApiInfo with LongKeyedMetaMapper[RemoteApiInfo] with CRUDify[Long, RemoteApiInfo]{
  override def dbTableName = "apis"
  override def fieldOrder = List(artifactId, version, createdAt)

  // crudify
  override def pageWrapper(body: NodeSeq) = <lift:surround with="admin" at="content">{body}</lift:surround>
  override def calcPrefix = List("admin",_dbTableNameLC)
  override def displayName = "Api"
  override def showAllMenuLocParams = LocGroup("admin") :: Nil
  override def createMenuLocParams = LocGroup("admin") :: Nil
  override def viewMenuLocParams = LocGroup("admin") :: Nil
  override def editMenuLocParams = LocGroup("admin") :: Nil
  override def deleteMenuLocParams = LocGroup("admin") :: Nil

  //TODO support special version (latest, ...)
  def findApiOf(artifactId: String, version: String): Box[RemoteApiInfo] = {
    //findAll().find(x => x.artifactId == artifactId && x.version == version) match {
    findAll(By(RemoteApiInfo.artifactId, artifactId), By(RemoteApiInfo.version, version)).headOption match {
      case Some(api) => Full(api)
      case None => Failure("api for " + artifactId + "::" + version + " is not registered")
    }
  }
  
  def findAllGGroupId() : List[String] = findMap(BySql("ggroup_id IS NOT NULL", IHaveValidatedThisSQL("davidB", "2010-11-19"))){x =>
    x.ggroupId.is match {
      case null => Empty
      case x if x.trim.length == 0 => Empty
      case x => Full(x.trim)
    }
  }.distinct // NotBy(RemoteApiInfo.ggroupId, null)
}

class RemoteApiInfo extends LongKeyedMapper[RemoteApiInfo] with IdPK with CreatedUpdated {
    def getSingleton = RemoteApiInfo
    
    private val txtFieldPattern = Pattern.compile("[A-Za-z0-9\\.\\-_]+")
    // fields
    object artifactId extends MappedString(this, 150) {
      override def validations = valMaxLen(maxLen, "too long : 150 max") _ :: valMinLen(3, "too short : 3 min") _:: valRegex(txtFieldPattern, "doesn't match pattern : " + txtFieldPattern.pattern) _ :: super.validations
      override def toForm = super.toForm.map( _ % ("required" -> "true") % ("pattern" -> txtFieldPattern.pattern))
    }
    object version extends MappedString(this, 25) {
      override def validations = valMaxLen(maxLen, "too long : 25 max") _ :: valMinLen(1, "too short : 1 min") _:: valRegex(txtFieldPattern, "doesn't match pattern : " + txtFieldPattern.pattern) _ :: super.validations
      override def toForm = super.toForm.map( _ % ("required" -> "true") % ("pattern" -> txtFieldPattern.pattern))
    }
    object url extends MappedString(this, 150) {
      override def toForm = super.toForm.map( _ % ("required" -> "true") % ("type" -> "url")) 
    }
    object format extends MappedApiProvider(this)
    object createdBy extends LongMappedMapper(this, User)
    object ggroupId extends MappedString(this, 150) {
      override def validations = valMaxLen(maxLen, "too long : 150 max") _ :: valMinLen(3, "too short : 3 min") _:: valRegex(txtFieldPattern, "doesn't match pattern : " + txtFieldPattern.pattern) _ :: super.validations
      override def toForm = super.toForm.map( _ % ("required" -> "true") % ("pattern" -> txtFieldPattern.pattern))
    }

    def provider : ApiProvider = format.is.asInstanceOf[ApiProviders.MyValue].ap

    def baseUrl = new URI(url.is)
}

abstract class MappedApiProvider[T <: Mapper[T]](owner: T) extends MappedEnum(owner, ApiProviders) {
  override def defaultValue = ApiProviders.vscaladoc2
}


object ApiProviders extends Enumeration {
  val javadoc2 = new MyValue(1, Javadoc2)
  val scaladoc = new MyValue(2, Scaladoc)
  val vscaladoc = new MyValue(3, VScaladoc)
  val scaladoc2 = new MyValue(4, Scaladoc2)
  val vscaladoc2 = new MyValue(5, VScaladoc2)

  class MyValue(id : Int, val ap : ApiProvider) extends Val(id, ap.toString)
}
