package net_alchim31_www

import javax.servlet.{FilterChain, ServletResponse, FilterConfig, ServletOutputStream}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServletResponseWrapper}
import java.io.{DataOutputStream, OutputStream, ByteArrayOutputStream, PrintWriter, OutputStreamWriter}

//TODO replace the filter by a better (optimization solution) like inserting into template and scalate renderer
//TODO use the same encoding for footer and main content
//TODO insert footer before </body>
class HtmlAppendFilter extends HttpFilter {
  private var _footer : Array[Byte] = null

  override def init(filterConfig : FilterConfig) {
    super.init(filterConfig)
    var footerStr = _filterConfig.getInitParameter("footer")
    if (footerStr != null) {
      footerStr = footerStr.trim
      if (footerStr.length > 0) {
        _footer = footerStr.getBytes
      }
    }
//    if (_footer != null) {
//      _footer = _footer + "</body></html>"
//    }
  }

  protected def doHttpFilter(request : HttpServletRequest, response : HttpServletResponse, chain : FilterChain) {
    if (_filterConfig == null) {
      return
    }

    val wrapper = new FilterResponseWrapper(response)
    chain.doFilter(request, wrapper)
    val abyte0 = wrapper.data
    if (abyte0 == null) {
      return
    }
    val servletoutputstream = response.getOutputStream()
    val ct = wrapper.getContentType()
//    println("check : " + ct + " ... " + response.getCharacterEncoding /*+ response.getHeader("Content-Type")*/ )
    if (_footer != null && ct != null && ct.indexOf("html") >= 0) {
      response.setContentType(ct);
      // wrong as length in Char != length in bytes
//      val s1 = new String(abyte0)
//      val i = math.max(s1.lastIndexOf("</BODY>"), s1.lastIndexOf("</body>"))
//      if (i < 0) {
//        servletoutputstream.write(abyte0)
//      } else {
//        response.setContentLength(i + _footer.length)
//        servletoutputstream.write(abyte0, 0, i);
//        servletoutputstream.write(_footer)
//        servletoutputstream.write(abyte0, i, abyte0.length - i);
//      }
        response.setContentLength(abyte0.length + _footer.length)
        servletoutputstream.write(abyte0)
        servletoutputstream.write(_footer) // not very good as encoding of footer can be != to html/abyte0 encoding
    } else {
      if (ct != null) {
        response.setContentType(ct)
      }
      response.setContentLength(abyte0.length)
      servletoutputstream.write(abyte0)
    }
    if (servletoutputstream != null) {
      servletoutputstream.close()
    }
  }

}

class FilterResponseWrapper(origResponse : HttpServletResponse) extends HttpServletResponseWrapper(origResponse) {
  private val output = new ByteArrayOutputStream()
  private var _writer : Option[PrintWriter] = None
  private var contentLength = 0
  private var contentType : String = _

  def data = {
    try {
      finishResponse()
    } catch {
      case e : Exception => () // ignore
    }
    output.toByteArray()
  }

  override def getOutputStream() : ServletOutputStream = new FilterServletOutputStream(output)

  override def getWriter() : PrintWriter = {
    if (!_writer.isDefined) {
      val servletoutputstream = getOutputStream()
      _writer = Some(new PrintWriter(new OutputStreamWriter(servletoutputstream, origResponse.getCharacterEncoding())))
    }
    _writer.get
  }

  // TODO use IOUtils
  private def finishResponse() {
    _writer.map(_.close())
    if (output != null) {
      output.flush();
      output.close();
    }
  }

  override def flushBuffer() {
    output.flush()
  }

  override def setContentLength(v : Int) {
    contentLength = v
    super.setContentLength(v)
  }

  def getContentLength() = contentLength

  override def setContentType(v : String) {
    contentType = v
    super.setContentType(v)
  }

  override def addHeader(name : String, value : String) {
    super.addHeader(name, value)
    if ("Content-Type".equalsIgnoreCase(name)) {
      setContentType(value)
    }
  }

  override def setHeader(name : String, value : String) {
    super.setHeader(name, value)
    if ("Content-Type".equalsIgnoreCase(name)) {
      setContentType(value)
    }
  }
  override def getContentType() = contentType

}

private class FilterServletOutputStream(outputstream : OutputStream) extends ServletOutputStream {
  private val _stream = new DataOutputStream(outputstream)

  def write(v : Int) = _stream.write(v)
  override def write(v : Array[Byte]) = _stream.write(v)
  override def write(v : Array[Byte], offset : Int, length : Int) = _stream.write(v, offset, length);
}
