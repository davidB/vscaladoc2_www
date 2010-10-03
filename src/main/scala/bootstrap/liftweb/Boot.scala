package bootstrap.liftweb

import net_alchim31_vscaladoc2_www.view.ApiView
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

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    initDB()
    
    // lets add Scalate
    //    val scalateView = new ScalateView
    //    scalateView.register
    LiftRules.statelessDispatchTable.append(ApiView.dispatch)

    // where to search snippet
    LiftRules.addToPackages("net_alchim31_vscaladoc2_www")


    // Build SiteMap
    def sitemap() = List( 
    		Menu("Home") / "index" >> LocGroup("public"),
    		Menu("Admin") / "admin" / "index" >> LocGroup("admin"),
    		Menu("Apis") / "admin" / "apis" >> LocGroup("admin")
    		  submenus(RemoteApiInfo.menus : _*)
//      // Menu with special Link
//      Menu(Loc("Static", Link(List("static"), true, "/static/index"),
//        "Static Content")) ::
      // Menu entries for the User management stuff
      ) ::: User.menus

    LiftRules.setSiteMap(SiteMap(sitemap : _*))

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)
    LiftRules.useXhtmlMimeType = false // recaptcha js lib
    
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }

  private def initDB() {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr
          "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }
    Schemifier.schemify(true, Schemifier.infoF _, User)
    Schemifier.schemify(true, Schemifier.infoF _, RemoteApiInfo)
    
    RemoteApiInfo.init()
    S.addAround(DB.buildLoanWrapper)
  }
}
