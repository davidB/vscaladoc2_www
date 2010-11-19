package net_alchim31_vscaladoc2_www.view

import net.liftweb.json.JsonAST.JInt
import net.liftweb.json.JsonAST.{JString, JNull}
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JArray
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.S
import net.liftweb.http.PostRequest
import net.liftweb.http.Req
import net.liftweb.http.LiftRules
import net_alchim31_vscaladoc2_www.AppServices
import net.liftweb.json._

 
object CommentsView extends RestHelper {
  serve {
    //case "comments" :: _ JsonPost json -> _ => {
    //case "comments" :: _ JsonPost _ => {
    //case "comments" :: _ JsonGet _ => {
    case r @ Req("comments" :: Nil, "json", _) => { 
      JObject(
        AppServices.commentSystem.findByRefPaths(S.params("refPaths[]")).toList.map{ kv =>
          val u = if (kv._2.nb > -1)  JString(kv._2.u.toString) else JNull
          JField(kv._1, JArray(List(JInt(kv._2.nb), u)))
        }
      )
    }
  }
}