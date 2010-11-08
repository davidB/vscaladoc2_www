CHANGES :
=========

## DONE
* admins can register api (create/update/delete) from several source/format : javadoc2, scaladoc, scaladoc2, vscaladoc2 (json)
* user can list api
* user can browse vscaladoc2 api (json) in html

## TODO, ideas
* write the announce
* document
  * title + description
  * how to use
  * planning
  * eg : url format, api laf, json format (to help complementary tool developers)
* promotion
  * nice product
  * write announce
* site look
  * change the font of the title
  * add a quick presentation text on home page
  * add links to legals, source, issue/feebacks, TODO/changelog,...
  * try other look for site page
  * may be integrate compass
  * api listing : display kind, logo, tags 
  * ranking of artifact
  * add page to search
    * artifact
    * Type
      * by name
      * by signature
    * method
      * by name
      * by signature  
  * add news
  * enable contact form (if user not logged => captcha)
  * add doc for users (api provider (syntax, running of genjson, register,...), laf provider, doctaglet provider, third-party/mashup tool)
* api laf
  * laf SPI
    * allow import/upload of laf (navigator/
    * simplify and document development
    * upload of css for selected laf
    * add support to test skin without remote server
      * provide a json sample
      * create a stand-alone local generator to convert json into static html site (like before).
  * default skin
    * document 
      * how to customize, test, submit patch, ...
    * navigator0
      * replace select box of package by a list of checkbox with link to package page
      * roundify
      * provide a chat (widget) (one room per artifact)
      * result from search filter should highlight (bold) matching fragment in the name
      * provide an index of methods (like javadoc, scaladoc2 (trunk))
    * entity0
      * provide logo info
      * change style of headers (h1,...) into description section
      * hide Any/AnyRef's methods
      * link display of
      * add helper.toInfo(uoa)
      * error sur display de methods (non available, non vscaladoc2) => link to origin
      * integrate a user comment support
        * extrenal service : disquss,...
        * collabdoc
        * home made (comments hide by default, number of comments, treeview, add (only for register user)) 
* api registration
  * register lift-2.2-M1
    * generate (with logo)
    * link
  * register scalatest
  * register vscaladoc2_genjson, vscaladoc2_www
  * register non vscaladoc2 api (javadoc2 for java lib, lead scala lib (specs, scalatest, ...)
  * allow registered user (or via captcha validation) to register remote api
  * add button to scan local api @local @admin
  * auto registration of artifacts from a info.ArtifactInfo (support for multi-module project/apis) to avoid long and annoying registration for every modules
* user registration
  * configure a MailServer (check email, and send forgotten password)
  * support user preferences
    * default navigator
    * default skin to use
    * default css to use
    * display inherited
    * favorite artifact  
* add tags supports
  * tags from source/json AND from user (restrited to owner or registered ?)
  * at artifact level
    * display in overview
    * display in listing (allow as filter)
  * at Type level
    * use as filter in listing
  * at Fieldext level
    * use as filter in listing
* add source link/display
  * support several mode :
    * link to external source via pattern with substitution by ${artifactId} ${version} ${filePath} ${lineNum} (suggest template for sxr, svn, github,...)
    * embedded : source are near json
  * www should display embedded source
  * use to display sample/demo/test code
* doctaglets
  * always display tags in the same order (eg : @deprecated @param @return @throws+@exception @author @version @since @see ...)
  * provide custom display for @tags (cloud, hide,...)
  * provide custom display for @todo @TODO
  * provide custom display for @deprecated
  * provide custom display for @see
  * use captcha for email (in author)
  * find a way to allow development and registration of doctaglet
  * create a graph view (interactive via applet, not interactive via image + link) for type relation, package dependencies,...
    * http://prefuse.org/doc/manual/
    * http://sourceforge.net/apps/trac/jung/wiki/JUNGManual 
* optimisation :
  * pbs :
    * failing http resources are not cached => done every time
    * too many http request to retreive origin json
    * too many entry into vscaladoc2/apis
  * use archive of *.json
    * use a backgound service (actor) to retrieve archive from original (avoid multi http request for one archive : queue)
    * unarchive into local apis cachedir and archive
    * store archive in persistant store
    * archive in .jar.gz (jar for store only no compression)
  * caching :
    * add etag (at servlet container or http front-end ?), see :	
      * http://www.infoq.com/articles/etags	
      * http://grepcode.com/file/repo1.maven.org/maven2/org.springframework/spring-web/3.0.1.RELEASE/org/springframework/web/filter/ShallowEtagHeaderFilter.java?av=f
      * http://code.google.com/p/remote-intent-architecture/source/browse/#svn/trunk/atom-server/src/servlet/etag	
      * http://www.caucho.com/resin/admin/http-proxy-cache.xtp	
    * cache (LFU) for instances  of uoa -> Box[info._]	
    * cache (LFU) for final web page (TTL 24H si version -SNAPSHOT) (may be use ehcache webfilter)	
    * scalate renderer could store already generated page (compressed + Etag)	
  * use compression filter gz [see](http://onjava.com/pub/a/onjava/2003/11/19/filters.html) @prod
  * benchmark
    * import/retrieve api from : FS, MySQL, couchdb
    * read json from  : raw, raw.gz (test on FS)
  * experiment :
    * full html templating in js (on client side : GWT, mustache, jquery-tmpl, ...)
* misc :
  * rss feed : comments, new/update about api entry, news
  * support api registration by archive upload
  * if user try to access unknow api, provide a form to register the api
  * generate Sitemap and submit updates to google [doc](http://www.google.com/support/webmasters/bin/answer.py?answer=183668)
  
## inspirations (to review)

* user's comments
  * http://www.scala-lang.org/node/7738
  * http://www.scala-lang.org/node/7097
* similar tools
  * doxygen
  * javadoc, scaladoc, extradoc
  * [JDocs](http://jdocs.com) , http://www.jdocs.com/httpclient/3.0.1/api-index.html?m=class&p=org.apache.commons.httpclient&c=ChunkedOutputStream&render=classic
  * [gotAPI](http://www.gotapi.com/contribute/index.html)
  * [colladoc](http://code.google.com/p/collaborative-scaladoc)
