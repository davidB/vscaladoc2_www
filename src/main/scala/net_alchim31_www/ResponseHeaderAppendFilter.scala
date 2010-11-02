package net_alchim31_www

import javax.servlet.FilterChain
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServletResponseWrapper}
import javax.servlet.FilterConfig
import javax.servlet.Filter
import scala.collection.JavaConversions._

/**
 * Filter use to inject (override) Http header.
 *
 * setHeader/addHeader should be call before OutputStream is closed
 * So place the filter before any Filter that write/flush/clode the Response outputStream.
 * {{{
 * #WEB-INF/web.xml
 *
 *  <filter>
 *   <filter-name>ClientCacheHeaderFilter</filter-name>
 *   <filter-class>net_alchim31_www.ResponseHeaderAppendFilter</filter-class>
 *   <init-param>
 *    <param-name>Cache-Control</param-name>
 *    <param-value>max-age=3600</param-value>
 *   </init-param>
 *   <init-param>
 *    <param-name>Pragma</param-name>
 *    <param-value></param-value>
 *   </init-param>
 *  </filter>
 * ...
 *  <filter-mapping>
 *   <filter-name>ClientCacheHeaderFilter</filter-name>
 *   <url-pattern>*.css</url-pattern>
 *  </filter-mapping>
 *  <filter-mapping>
 *   <filter-name>ClientCacheHeaderFilter</filter-name>
 *   <url-pattern>*.js</url-pattern>
 *  </filter-mapping>
 * }}}
 *
 * @author david.bernard
 * @basedon http://onjava.com/pub/a/onjava/2004/03/03/filters.html
 */
class ResponseHeaderAppendFilter extends HttpFilter {

  def doHttpFilter(request : HttpServletRequest, response : HttpServletResponse, chain : FilterChain) {

    val wrapper = response //new HeaderFilterResponseWrapper(response)
    chain.doFilter(request, wrapper)
    for (name <- _filterConfig.getInitParameterNames()) {
      val headerName = name.asInstanceOf[String]
      //response.addHeader(headerName, _filterConfig.getInitParameter(headerName))
      wrapper.setHeader(headerName, _filterConfig.getInitParameter(headerName))
    }
    //wrapper.flush()
  }
}

////TODO support concurrent update of headers
//class HeaderFilterResponseWrapper(origResponse : HttpServletResponse) extends HttpServletResponseWrapper(origResponse) {
//  import scala.collection.mutable
//
//  private val _headers = new mutable.HashMap[String, String]()
//
//  override def addHeader(name : String, value : String) = setHeader(name, value)
//
//  override def setHeader(name : String, value : String) {
//    if (value == null || value.trim.length == 0) {
//      _headers -= name
//    } else {
//      _headers += (name -> value)
//    }
//  }
//
//  override def containsHeader(name : String) = _headers.contains(name)
//
//  def flush() {
//    for ((name, value) <- _headers) {
//     println("add", name, value)
//     super.setHeader(name, value)
//    }
//  }
//}