package net_alchim31_vscaladoc2_www.model

import net.liftweb.mapper.CreatedUpdated
import net.liftweb.sitemap.Loc.LocGroup
import scala.xml.NodeSeq
import net.liftweb.mapper.CRUDify
import java.util.Locale
import java.util.Properties
import net.tanesha.recaptcha.ReCaptchaImpl
import net.tanesha.recaptcha.ReCaptchaFactory
import scala.xml.XML
import net.liftweb.http.S
import net.liftweb.util.FieldError
import net.liftweb.mapper.MappedPostalCode
import net.liftweb.mapper.MappedCountry
import net.liftweb.mapper.MappedTextarea
import net.liftweb.mapper.MegaProtoUser
import net.liftweb.mapper.MetaMegaProtoUser
import net.liftweb.common.{Box, Full, Empty, Failure}

/*
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
*/
// TODO : Captcha integration (ReCaptcha)
// * http://groups.google.com/group/liftweb/browse_thread/thread/6188f67398414268/606b0480d7dde507?lnk=gst&q=recaptcha#606b0480d7dde507
// * http://code.google.com/apis/recaptcha/docs/java.html
// * reCaptcha info for alchim31 https://www.google.com/recaptcha/admin/site?siteid=314371730

// TODO : GeoLocalisation of JdR player
// * http://code.google.com/apis/maps/documentation/geocoding/

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaProtoUser[User] with ReCaptcha /* with CRUDify[Long, User]*/ {
//
//  // crudify
//  override def pageWrapper(body: NodeSeq) = <lift:surround with="admin" at="content">{body}</lift:surround>
//  override def calcPrefix = List("admin",_dbTableNameLC)
//  override def displayName = "Api"
//  override def showAllMenuLocParams = LocGroup("admin") :: Nil
//  override def createMenuLocParams = LocGroup("admin") :: Nil
//  override def viewMenuLocParams = LocGroup("admin") :: Nil
//  override def editMenuLocParams = LocGroup("admin") :: Nil
//  override def deleteMenuLocParams = LocGroup("admin") :: Nil
//
//  override lazy val editPath : List[String] = super.asInstanceOf[MetaMegaProtoUser[User]].editPath
//  override def menus  = super.asInstanceOf[MetaMegaProtoUser[User]].menus
//  def menusCrud = super.asInstanceOf[CRUDify[Long, User]].menus
  
  override def dbTableName = "users" // define the DB table name
  override def screenWrap = Full(<lift:surround with="default" at="content">
                                   <lift:bind/>
                                 </lift:surround>)
  override def signupFields = fieldOrder //TODO append captcha

  // define the order fields will appear in forms and output
  override def fieldOrder = List(firstName, lastName, email, locale, timezone, password)

  // TODO comment this line out to require email validations
  override def skipEmailValidation = true

  override def validateSignup(user: User): List[FieldError] = validateCaptcha() ::: super.validateSignup(user)
  override def signupXhtml(user: User) = {
    (<form method="post" action={ S.uri }>
       <table>
         <tr><td colspan="2">{ S.??("sign.up") }</td></tr>
         { localForm(user, false) }
         <tr><td>&nbsp;</td><td>{ captchaXhtml() }</td></tr>
         <tr><td>&nbsp;</td><td><user:submit/></td></tr>
       </table>
     </form>)
  }

}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends MegaProtoUser[User] with CreatedUpdated {
  def getSingleton = User // what's the "meta" server

//    object country extends MappedCountry(this) {
//  	  override def defaultValue =  S.containerRequest.flatMap(_.locale).getOrElse(Locale.getDefault).getDisplayCountry
//    }
//    object postalCode extends MappedPostalCode(this, this.country)

}

