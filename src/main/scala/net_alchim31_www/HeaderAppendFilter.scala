package net_alchim31_www

import javax.servlet.FilterChain
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import javax.servlet.FilterConfig
import javax.servlet.Filter
import collection.JavaConversions._

class ResponseHeaderAppendFilter extends HttpFilter {

  def doHttpFilter(request : HttpServletRequest, response : HttpServletResponse, chain : FilterChain) {
    // set the provided HTTP response parameters
    for ( name <- _filterConfig.getInitParameterNames()) {
      val headerName = name.asInstanceOf[String]
      response.addHeader(headerName, _filterConfig.getInitParameter(headerName))
    }
    // pass the request/response on
    chain.doFilter(request, response);
  }
}