package net_alchim31_vscaladoc2_www.snippet

import net_alchim31_vscaladoc2_www.model.User
import net_alchim31_vscaladoc2_www.AppServices
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net_alchim31_vscaladoc2_www.model.RemoteApiInfo
import scala.xml.NodeSeq
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.util.Helpers._
import net.liftweb.common.{Full,Box,Empty,Failure,Loggable}

class ApiAdd extends Loggable {
  
 
  def add(xhtml : NodeSeq) : NodeSeq = {
   var holder = RemoteApiInfo.create

   def checkAndSave() {
     holder.validate match {
      case Nil => {
        val newEntry = holder
        newEntry.createdBy(User.currentUser)
//        holder = RemoteApiInfo.create
//        
//        (RemoteApiInfo.create
//          .artifactId(holder.artifactId.is)
//          .version(holder.version.is)
//          .url(holder.url.is)
//          .format(holder.format.is)
//        )
        println(newEntry.url.is)
        AppServices.apis.register(newEntry)//holder.save ; S.notice("Added "+todo.desc)
        S.notice("Added " + newEntry.artifactId.is + "-" + newEntry.version.is)
      }
      case xs => S.error(xs) ; S.mapSnippet("ApiAdd.add", doBind)
     }
   }

   def doBind(form: NodeSeq) = {
     bind("form", form,
        "artifactId" -%> holder.artifactId.toForm,
        "version" -%> holder.version.toForm,
        "format" -%> holder.format.toForm,
        "url" -%>  holder.url.toForm,
        "submit" -> SHtml.submit("Add", checkAndSave _))  ++ SHtml.hidden(checkAndSave _)
   }
   
   doBind(xhtml)    
  }
}