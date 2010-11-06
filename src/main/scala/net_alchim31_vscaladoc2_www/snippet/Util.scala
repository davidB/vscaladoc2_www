package net_alchim31_vscaladoc2_www.snippet

import net_alchim31_vscaladoc2_www.model.User
import scala.xml.{NodeSeq} 

class Util { 
  def loggedIn(html: NodeSeq) = 
    if (User.loggedIn_?) html else NodeSeq.Empty 
  
  def loggedOut(html: NodeSeq) = 
    if (!User.loggedIn_?) html else NodeSeq.Empty 
} 