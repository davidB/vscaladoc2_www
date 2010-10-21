package net_alchim31_vscaladoc2_www.model

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

  def init() {
	if (RemoteApiInfo.find() == Empty){
 	val data = List(
 			("sample", "1.0.0", new URL("file:///tmp/sample-api"), ApiProviders.vscaladoc2),
 			("vscaladoc_demoprj", "0.1-SNAPSHOT", new URL("file:///home3/dwayne/tmp/vscaladoc_demoprj/0.1-SNAPSHOT"), ApiProviders.vscaladoc2),
 			("scala-library", "2.8.0", new URL("http://www.scala-lang.org/api/2.8.0/index.html"), ApiProviders.scaladoc2),
 			("scala-library", "2.7.7", new URL("http://www.scala-lang.org/api/2.7.7/index.html"), ApiProviders.scaladoc)
    ).foreach { x =>
 		val v: RemoteApiInfo = RemoteApiInfo.create
 		v.artifactId(x._1)
 		v.version(x._2)
 		v.url(x._3.toExternalForm)
 		v.format(x._4)
 		v.save
	  }
	}
  }
}

class RemoteApiInfo extends LongKeyedMapper[RemoteApiInfo] with IdPK with CreatedUpdated {
    def getSingleton = RemoteApiInfo
    // fields
    object artifactId extends MappedString(this, 150)
    object version extends MappedString(this, 25)
    object url extends MappedString(this, 150)
    object format extends MappedApiProvider(this)
    //object createdBy extends LongMappedMapper(this, User)

    def provider : ApiProvider = format.is.asInstanceOf[ApiProviders.MyValue].ap

    def baseUrl = new URL(url.is)
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