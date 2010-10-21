package net_alchim31_vscaladoc2_www

import java.io.FileReader
import java.net.URI
import net.liftweb.json.JsonParser
import net_alchim31_vscaladoc2_www.model.RemoteApiInfo
import net_alchim31_vscaladoc2_www.info._
import net.liftweb.common.{ Box, Full, Empty, Failure }
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.JsonAST._

trait RawDataProvider {
  type UOA = String
  def find(uoa: Uoa): Box[JValue]
}

class RawDataToInfo(val rdp: RawDataProvider, val uoaHelper : UoaHelper) {
  implicit val formats = net.liftweb.json.DefaultFormats // Brings in default date formats etc.

  def toArtifactInfo(uoa: Uoa4Artifact): Box[ArtifactInfo] = {
    for (jv <- rdp.find(uoa)) yield {
      new ArtifactInfo() {
        override val artifactId = (jv \ "artifactId").extract[String]
        override val version = (jv \ "version").extract[String]
        override val description = (jv \ "description").extract[String]
      }
    }
  }

  protected def toTypeInfo(jo : JObject): Box[TypeInfo] = {
	  Empty
//	new TypeInfo() {
//    def url : URI = new URI("api://")
//    val simpleName: String = (jo \ "name").extract[String]
//    def signature: String = fqName
//    val description: HtmlString = (jo \ "description").extract[HtmlString]
//    def docTags: Seq[DocTag] = Nil
//    val fqName: String = (jo \ "qualifiedName").extract[String]
//    val kind: String = (jo \ "kind").extract[String]
//    var companion: Option[TypeInfo] = None
//    def isInherited(m: MemberInfo) = false
//    def constructors: List[MemberInfo] = Nil
//    def fields: List[MemberInfo] = Nil // (jo\ "values").extract[List[String]].flatMap(x => toFieldExt(uoaHelper(x)))
//    def methods: List[MemberInfo] = Nil // (jo\ "methods").extract[List[String]].flatMap(x => toFieldExt(uoaHelper(x)))
//  }
  }

  def toTypeInfo(uoa: Uoa4Type): List[Box[TypeInfo]] = {
    rdp.find(uoa) match {
    	case x : Failure => List(x)
    	case Empty => Nil
    	case Full(jv) => {
    	  for(jo <- (jv \ "e").extract[List[JObject]]) yield toTypeInfo(jo)
      }
    }
  }

  def findAllTypes(uoa: Uoa4Package): List[Box[Uoa4Type]] = {
    rdp.find(uoa) match {
    	case x : Failure => List(x)
    	case Empty => Nil
    	case Full(jv) => {
    		val list = for (JField("members", JArray(list)) <- (jv \ "e" \ "members")) yield list
    		println(list)
		    list.flatten.distinct.flatMap { refPath =>
		      refPath match {
		     	  case JString(s) => uoaHelper(s) match {
		     	 	case Full(uoa) => uoa match {
		     	 	  case u : Uoa4Package => findAllTypes(u)
		    	      case u : Uoa4Type => List(Full(u))
                      case x => println("found :" + x); Nil //ignore
		     	 	}
		     	 	case x: Failure => List(x)
		     	 	case Empty => List(Empty)
		     	  }
		     	  case x => List(Failure("expected JString, found " + x)) //ignore
		      }
		    }
    	}
    }
  }
}

//TODO cache result of remote request
//TODO download archive, store in DB, unarchive in local FS cache
class BasicRawDataProvider() extends RawDataProvider {
  import dispatch._
  import Http._

  private val _http = new Http

  def find(uoa: Uoa): Box[JValue] = {
	  for( url <- toUrl(uoa)) yield {
	 	println("url : " + url )
	 	url.startsWith("file:/") match {
	 		case true => JsonParser.parse(new FileReader(new URI(url).getPath))
	 		case false => _http(new Request(url) >- { s => JsonParser.parse(s)})
	 	}

	  }
  }

  def toUrl(uoa : Uoa) : Box[String] = uoa match {
	case Uoa4Artifact(artifactId, version) => {
	  for (
		rai <- RemoteApiInfo.findApiOf(artifactId, version);
		rpath <- rai.provider.rurlPathOf()
	  ) yield rai.baseUrl.toExternalForm + rpath
	}
	case Uoa4Package(packageName, Uoa4Artifact(artifactId, version)) => {
	  for (
		rai <- RemoteApiInfo.findApiOf(artifactId, version);
		rpath <- rai.provider.rurlPathOf(packageName)
	  ) yield rai.baseUrl.toExternalForm + rpath
	}
	case Uoa4Type(typeName, Uoa4Package(packageName, Uoa4Artifact(artifactId, version))) => {
	  for (
		rai <- RemoteApiInfo.findApiOf(artifactId, version);
		rpath <- rai.provider.rurlPathOf(packageName, typeName)
	  ) yield rai.baseUrl.toExternalForm + rpath
	}
	case Uoa4Fieldext(fieldextName, Uoa4Type(typeName, Uoa4Package(packageName, Uoa4Artifact(artifactId, version)))) => {
	  for (
		rai <- RemoteApiInfo.findApiOf(artifactId, version);
		rpath <- rai.provider.rurlPathOf(packageName, typeName, fieldextName)
	  ) yield rai.baseUrl.toExternalForm + rpath
	}
  }
}