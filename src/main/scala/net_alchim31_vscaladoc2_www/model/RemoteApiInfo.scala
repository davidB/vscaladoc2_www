package net_alchim31_vscaladoc2_www.model

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
      case None => println("f0"); Failure("api for " + artifactId + "::" + version + " is not registered")
    }
  }
  
  def init() {
	if (RemoteApiInfo.find() == Empty){
 	val data = List(
 			("sample", "1.0.0", new URL("file://tmp/sample-api"), VScaladoc2),
 			("scala-library", "2.8.0", new URL("http://www.scala-lang.org/api/2.8.0/index.html"), Scaladoc2),
 			("scala-library", "2.7.7", new URL("http://www.scala-lang.org/api/2.7.7/index.html"), Scaladoc)
    ).foreach { x =>
 		val v: RemoteApiInfo = RemoteApiInfo.create
 		v.artifactId(x._1)
 		v.version(x._2)
 		v.url(x._3.toExternalForm)
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
    //object createdBy extends LongMappedMapper(this, User)
    
    def provider : ApiProvider = Scaladoc2
    def provider_=( v : ApiProvider) = {}
    
    def baseUrl = new URL(url.is)
}

