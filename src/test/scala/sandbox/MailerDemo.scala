/**
 * 
 */
package sandbox

/**
 * @author david.bernard
 *
 */
object MailerDemo {

  def main(args: Array[String]) {
    import net.liftweb.util.{Mailer, Props}
    import Mailer._
    //Mailer read properties from props/xxxx.props
    println("stmp.host :" + Props.get("mail.smtp.host"))
    Mailer.authenticator = Props.get("mail.smtp.auth.login").map { user =>
      new javax.mail.Authenticator() {
        override def getPasswordAuthentication() = new javax.mail.PasswordAuthentication(user, Props.get("mail.smtp.auth.password").openOr(""))
      }
    }
    Mailer.sendMail(From("xxx"), Subject("Just a test"), To(Props.get("mail.to").openOr("zzz")), PlainMailBodyType("test form MailerDemo"))
    println("Send")
  }
}