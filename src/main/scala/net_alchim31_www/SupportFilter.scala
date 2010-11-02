package net_alchim31_www

import javax.servlet.{FilterConfig, Filter, FilterChain, ServletResponse, ServletRequest}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

trait  HttpFilter extends Filter {
  protected var _filterConfig : FilterConfig = _

  def init(filterConfig : FilterConfig) {
    _filterConfig = filterConfig
  }

  final def doFilter(request : ServletRequest, response : ServletResponse, chain : FilterChain) {
    doHttpFilter(request.asInstanceOf[HttpServletRequest], response.asInstanceOf[HttpServletResponse], chain)
  }

  protected def doHttpFilter(request : HttpServletRequest, response : HttpServletResponse, chain : FilterChain)


  def destroy() {
    _filterConfig = null
  }

}