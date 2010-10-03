package net_alchim31_vscaladoc2_www.model

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
object User extends User with MetaMegaProtoUser[User] {
  override def dbTableName = "users" // define the DB table name
  override def screenWrap = Full(<lift:surround with="default" at="content">
                                   <lift:bind/>
                                 </lift:surround>)
  override def signupFields = fieldOrder //TODO append captcha

  // define the order fields will appear in forms and output
  override def fieldOrder = List(firstName, lastName, email,
    locale, timezone, password, textArea)

  // comment this line out to require email validations
  //override def skipEmailValidation = true

  // add ReCaptcha  
  // ReCaptcha js lib require  LiftRules.useXhtmlMimeType = false  
  // override localForm instead of signupXhtml if you want to use captcha for every user edition
  protected def reCaptchadomainName = "alchim31.net"  
  protected def reCaptchaPublicKey = "6LeS7rwSAAAAALrSdcBKkCz5WGBMSK0PuejBdQaB"
  protected def reCaptchaPrivateKey = "6LeS7rwSAAAAAGN3R5A8QGkN5UDnVA-uIY4jFv8s"
	  
  private lazy val reCaptcha = ReCaptchaFactory.newReCaptcha(reCaptchaPublicKey, reCaptchaPrivateKey, false)

  private def captchaXhtml() = {
<script type="text/javascript">
var RecaptchaOptions = {"{theme:'white',lang:'" + S.containerRequest.flatMap(_.locale).map(_.getLanguage).getOrElse("en") + "'}"};
</script>
<script type="text/javascript" src={"http://api.recaptcha.net/challenge?k=" + reCaptchaPublicKey}></script>	  
//	val props = new Properties()
//	props.setProperty("theme", "white")
//	props.setProperty("lang", S.containerRequest.flatMap(_.locale).map(_.getLanguage).getOrElse("en"))
//    val x = reCaptcha.createRecaptchaHtml(null, props)
//    println(x)
//    XML.loadString(x)
  }
  
  private def validateCaptcha(): List[FieldError] = {
    val res = for (
      remoteAddr <- S.containerRequest.map(_.remoteAddress);
      challenge <- S.param("recaptcha_challenge_field");
      uresponse <- S.param("recaptcha_response_field")
    ) yield {
      val reCaptchaResponse = reCaptcha.checkAnswer(remoteAddr, challenge, uresponse)
      reCaptchaResponse.isValid() match {
        case true => Nil
        case false => print("Answer is wrong"); Nil
      }
    }
    res match {
      case Failure(msg, _, _) => Nil
      case Full(msg) => Nil
      case Empty => Nil
    }
  }
  
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
class User extends MegaProtoUser[User] {
  def getSingleton = User // what's the "meta" server

  //  object country extends MappedCountry(this) {
  //	  override def defaultValue = Locale.getDefault.getDisplayCountry
  //  }
  //  object postalCode extends MappedPostalCode(this, this.country)

  // define an additional field for a personal essay
  object textArea extends MappedTextarea(this, 2048) {
    override def textareaRows = 10
    override def textareaCols = 50
    override def displayName = "Personal Essay"
  }
}

