package net_alchim31_vscaladoc2_www.model
/**
 * To use ReCaptach (http://www.google.com/recaptcha) :
 * * add a project dependency
 *   <dependency>
 *     <groupId>net.tanesha.recaptcha4j</groupId>
 *     <artifactId>recaptcha4j</artifactId>
 *     <version>0.0.7</version>
 *   </dependency>
 * * mix the trait with your MetaMegaProtoUser object and link both
 *<pre>
 *  object User extends User with XxxxMetaMegaProtoUser[User] with ReCaptcha{
 *
 *  ...
 *
 *  protected def reCaptchaPublicKey = ...
 *  protected def reCaptchaPrivateKey = ...
 *
 *  // protected override def reCaptchaOptions  = (super.reCaptchaOptions merge ("theme" -> "blackglass")).asInstanceOf[JObject]

 *  override def validateSignup(user: User): List[FieldError] = validateCaptcha() ::: super.validateSignup(user)
 *  // override localForm instead of signupXhtml if you want to use captcha for every user edition
 *  override def signupXhtml(user: User) = {
 *   (<form method="post" action={ S.uri }>
 *      <table>
 *        <tr><td colspan="2">{ S.??("sign.up") }</td></tr>
 *        { localForm(user, false) }
 *        <tr><td>&nbsp;</td><td>{ captchaXhtml() }</td></tr>
 *        <tr><td>&nbsp;</td><td><user:submit/></td></tr>
 *      </table>
 *    </form>)
 *  }
 *</pre>
 * * PublicKey and PrivateKey are provide when you register you domainName to ReCaptcha
 *   you could hardcode value, or retrieve them from configuration (e.g. if you use Lift configuration :
 *<pre>
 *  import net.liftweb.util.Props
 *
 *  Props.requireOrDie("reCaptcha.publicKey", "reCaptcha.privateKey")
 *  protected val reCaptchaPublicKey = Props.get("reCaptcha.publicKey").get
 *  protected val reCaptchaPrivateKey = Props.get("reCaptcha.privateKey").get
 *</pre>
 * * add localisation messages for keys (add under src/main/resources/i18n/recaptcha_xxxx.properties)
 * * modify Boot.scala
 * <pre>
 *   LiftRules.useXhtmlMimeType = false //required by ReCaptcha js lib
 *   LiftRules.resourceNames = "recaptcha" :: LiftRules.resourceNames
 * </pre>
 * * see http://code.google.com/apis/recaptcha/docs/java.html for troubleshooting (DNS,...), customization (themes), ...
 *
 * @TODO internationalized error message
 * @TODO create a "real" Lift Field for captcha
 * @author david.bernard
 */
trait ReCaptcha {
  import net.liftweb.common.{Box, Empty, Full, Failure}
  import net.liftweb.util.{FieldError, FieldIdentifier}
  import net.liftweb.http.S
  import net.tanesha.recaptcha.ReCaptchaFactory
  import net.liftweb.json.JsonAST
  import net.liftweb.json.JsonDSL._
  import net.liftweb.util.Props
  import scala.xml.NodeSeq


  // add ReCaptcha
  /**
   * Define the public key to used to connect to reCapcha service.
   * Default implementation retrieve value from property "recaptcha.publicKey".
   */
  protected def reCaptchaPublicKey : Box[String] = Props.get("recaptcha.publicKey")

  /**
   * Define the private key to used to connect to reCapcha service
   * Default implementation retrieve value from property "recaptcha.privateKey".
   */
  protected def reCaptchaPrivateKey : Box[String] = Props.get("recaptcha.privateKey")

  /**
   * Define the option to configure reCaptcha widget.
   *
   * @see http://code.google.com/apis/recaptcha/docs/customization.html to have the list possible customization
   * @return the javascript option map (as JObject)
   * @codeAsDoc
   */
  protected def reCaptchaOptions = ("theme" -> "white") ~ ("lang" -> S.containerRequest.flatMap(_.locale).map(_.getLanguage).getOrElse("en"))

  private lazy val reCaptcha = {
	for (reCaptchaPublicKey <- reCaptchaPublicKey; reCaptchaPrivateKey <- reCaptchaPrivateKey) yield {
	  ReCaptchaFactory.newReCaptcha(reCaptchaPublicKey, reCaptchaPrivateKey, false)
	}
  }

  /**
   * @return captchaXhtml(publicKey) if reCaptchaPublicKey is Full(publicKey) else return NodeSeq.Empty
   * @codeAsDoc
   */
  protected def captchaXhtml() : NodeSeq = reCaptchaPublicKey.map( x => captchaXhtml(x)).getOrElse(NodeSeq.Empty)

  protected def captchaXhtml(publicKey : String) : NodeSeq = {
    import scala.xml.Unparsed
    import net.liftweb.http.js.JE
    import net.liftweb.json.JsonAST._

    val RecaptchaOptions = compact(render(reCaptchaOptions))
    <xml:group>
      <script>
        var RecaptchaOptions = {Unparsed(RecaptchaOptions)};
      </script>
      <script type="text/javascript" src={"http://api.recaptcha.net/challenge?k=" + publicKey}></script>      <noscript>        <iframe src={"http://www.google.com/recaptcha/api/noscript?k=" + publicKey} height="300" width="500" frameborder="0"></iframe><br/>        <textarea name="recaptcha_challenge_field" rows="3" cols="40">        </textarea>        <input type="hidden" name="recaptcha_response_field" value="manual_challenge"/>      </noscript>
    </xml:group>
  }

  protected def validateCaptcha(): List[FieldError] = {
    def checkAnswer(remoteAddr : String, challenge : String, uresponse : String) : Box[String]= {
      reCaptcha.flatMap { x =>
        val reCaptchaResponse = x.checkAnswer(remoteAddr, challenge, uresponse)
        reCaptchaResponse.isValid() match {
          case true => Empty
          case false => Full(S.?("reCaptcha." + reCaptchaResponse.getErrorMessage))
        }
      }
    }
    val res = for (
      remoteAddr <- S.containerRequest.map(_.remoteAddress);
      challenge <- S.param("recaptcha_challenge_field");
      uresponse <- S.param("recaptcha_response_field") ;
      b <- checkAnswer(remoteAddr, challenge, uresponse)
    ) yield b

    res match {
      case Failure(msg, _, _) => List(FieldError(FakeFieldIdentifier(Full("reCaptcha")), msg))
      case Full(msg) => List(FieldError(FakeFieldIdentifier(Full("reCaptcha")), msg))
      case Empty => Nil
    }
  }

  case class FakeFieldIdentifier(override val uniqueFieldId : Box[String]) extends FieldIdentifier

}
