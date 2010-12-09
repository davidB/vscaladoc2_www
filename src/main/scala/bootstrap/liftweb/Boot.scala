package bootstrap.liftweb

import javax.sql.DataSource
import javax.naming.Context
import javax.naming.InitialContext
import net.liftweb.mapper.MapperRules
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{ DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor }
import _root_.java.sql.{ Connection, DriverManager }
import _root_.net_alchim31_vscaladoc2_www.model._
import net_alchim31_vscaladoc2_www.AppServices
import net_alchim31_vscaladoc2_www_gcomments.GCommentsInfoMapped
import net_alchim31_vscaladoc2_www.view.CommentsView
import net_alchim31_vscaladoc2_www.view.ApiView

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends Loggable {
  def boot {
    logger.debug("LogMode : DEBUG MODE ENABLED!")
    logger.info("cfg : RunMode : " + Props.mode)
    logger.info("cfg : user.home = " + System.getProperty("user.home"))

    // where to search snippet
    LiftRules.addToPackages("net_alchim31_vscaladoc2_www")
    
    configureEmail()
    
    configureRDBMS()

    configureHttpRequest()

    configureUserExperience()
  }

  private def configureEmail() {
    import net.liftweb.util.{ Mailer, Props }
    import Mailer._
    //Mailer read properties from props/xxxx.props
    Mailer.authenticator = Props.get("mail.smtp.auth.login").map { user =>
      new javax.mail.Authenticator() {
        override def getPasswordAuthentication() = new javax.mail.PasswordAuthentication(user, Props.get("mail.smtp.auth.password").openOr(""))
      }
    }
    for (mailto <- Props.get("mail.to")) {
      Mailer.sendMail(From("VScaladoc"), Subject("Just a test"), To(mailto), PlainMailBodyType("server started"))
    }
  }

  private def configureRDBMS() {
    DefaultConnectionIdentifier.jndiName = "jdbc/vscaladoc2"
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(
          Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"),
          Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    MapperRules.columnName = (_, name) => StringHelpers.snakify(name)
    MapperRules.tableName = (_, name) => StringHelpers.snakify(name)

    Schemifier.schemify(true, Schemifier.infoF _, User, RemoteApiInfo, GCommentsInfoMapped)

    //    RemoteApiInfo.init()

    S.addAround(DB.buildLoanWrapper)
  }

  private def configureHttpRequest() {
    // Build SiteMap
    val MustBeLoggedIn = If(() => User.loggedIn_?, "")
    val IsAdmin = If(() => { val b = User.currentUser.map(_.id.is <= 1).getOrElse(false); println("check", b, User.currentUser.map(_.id.is)); b }, "")
    def sitemap() = List(
      Menu("Home") / "index",
      Menu("Blank") / "blank" >> Hidden,
      Menu("About") / "about" / ** >> Hidden,
      Menu("Admin") / "admin" / "index" >> IsAdmin
      submenus (RemoteApiInfo.menus : _*)
      submenus (GCommentsInfoMapped.menus : _*) //      // Menu with special Link
//      Menu(Loc("Static", Link(List("static"), true, "/static/index"),
//        "Static Content")) ::
// Menu entries for the User management stuff
) ::: User.menus

    LiftRules.setSiteMap(SiteMap(sitemap : _*))

    // setup the 404 handler
    //    LiftRules.uriNotFound.prepend(NamedPF("404handler"){
    //      case (req,failure) => NotFoundAsTemplate(ParsePath(List("404"),"html",false,false))
    //    })

    // lets add Scalate
    //    val scalateView = new ScalateView
    //    scalateView.register
    AppServices.init()
    LiftRules.statelessDispatchTable.append(ApiView.dispatch)
    LiftRules.dispatch.append(CommentsView) //statefull
    //LiftRules.statelessDispatchTable.append(CommentsView)

    // make requests utf-8, html
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))
    LiftRules.useXhtmlMimeType = false // recaptcha js lib
  }

  private def configureUserExperience() {

    // set the time that notices should be displayed and then fadeout
    LiftRules.noticesAutoFadeOut.default.set((notices : NoticeType.Value) => Full(2 seconds, 2 seconds))

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart = Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    LiftRules.ajaxEnd = Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)
  }
}
