package net_alchim31_www

import javax.servlet.ServletContextEvent
import javax.servlet.ServletContextListener
import scala.collection.JavaConversions._

class ContextAttAsSysProperty extends ServletContextListener {
  def contextInitialized(sce : ServletContextEvent) {
    val ctx = sce.getServletContext
    for (k <- ctx.getInitParameterNames) {
      val name = k.asInstanceOf[String]
      System.setProperty(name, ctx.getInitParameter(name))
    }
  }

  def contextDestroyed(sce : ServletContextEvent) {
  }
}