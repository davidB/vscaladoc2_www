package net_alchim31_vscaladoc2_www
import java.net.URI

/**
 * Comments ignore version of artifacts => every request ignore version
 * 
 * @author david.bernard
 */
trait CommentSystem {
  /**
   * @param nb number of messages , -1 => no backend
   * @param u if nb > 0 then uri of the html page to display message.
   *   else if nb == 0 then uri of tool to send/post first message
   *   else if nb < 0, could ignored (anything) 
   */
  case class Info(nb : Int, u : URI)
  
  /**
   * @param refPaths
   * @return a map with key are values from refPath
   */
  def findByRefPaths(refPaths : Seq[String]) : Map[String, Info]
  
  def init()
}




/*
 * TEST :
 * 
 * * post first from web
 * * post first from email client
 * * reply from email client
 * * reply from web
 * * post about artifact, package, type, fieldext (homonyme)
 * * check there is right number of messages
 * * check there is right display of messages
 * * 
 */