import _root_.org.mortbay.jetty.Connector
import _root_.org.mortbay.jetty.Server
import _root_.org.mortbay.jetty.webapp.WebAppContext
import org.mortbay.jetty.nio._
//import _root_.org.mortbay.jetty.plus

object RunWebApp {
  def main(args: Array[String]) {
    val server = new Server
    val scc = new SelectChannelConnector
    scc.setPort(8080)
    server.setConnectors(Array(scc))

//    val ds = new org.h2.jdbcx.JdbcDataSource()
//    ds.setURL("jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE")
//    ds.setUser("sa")
//    ds.setPassword("sa")
//    val confJndi = new plus.webapp.Configuration()
//    confJndi.bindEnvEntry("jdbc/DSTest", ds)//new plus.naming.Resource("jdbc/DSTest", ds))

    val context = new WebAppContext()
//    context.setConfigurations(Array(confJndi))
    context.setServer(server)
    context.setContextPath("/")
    context.setWar("src/main/webapp")

    server.addHandler(context)

    try {
      println(">>> STARTING EMBEDDED JETTY SERVER, PRESS ANY KEY TO STOP")
      server.start()
      while (System.in.available() == 0) {
        Thread.sleep(5000)
      }
      server.stop()
      server.join()
    } catch {
      case exc: Exception => {
        exc.printStackTrace()
        System.exit(100)
      }
    }
  }
}
