package net_alchim31_vscaladoc2_www

import net.sf.ehcache.CacheManager


import net.liftweb.common.{Full,Box,Empty,Failure,Loggable}
import net_alchim31_vscaladoc2_www_gcomments.{GCommentsService, UrlMaker4GComments} 

//TODO remove LiftRules / S dependencies
object AppServices extends Loggable {
  import net.liftweb.http.S
  import java.net.URI
  import net_alchim31_utils.FileSystemHelper
  import net.liftweb.http.LiftRules
  import java.io.File

  lazy val workdir = {
    var rootdir = new File(System.getProperty("user.home"), ".config/vscaladoc2")
    if (!rootdir.exists && !rootdir.mkdirs()) {
      rootdir = new File(LiftRules.context.attribute("javax.servlet.context.tempdir").getOrElse(System.getProperty("java.io.tmp")).toString, "vscaladoc2")
      rootdir.mkdirs()
    }
    //val tmpDirPath = LiftRules.context.attribute("javax.servlet.context.tempdir").getOrElse("/home/dwayne/work/oss/vscaladoc2_www/src/main")
    logger.info("workdir : " + rootdir + " ... " + rootdir.exists)
    rootdir
  }

  lazy val fsh = new FileSystemHelper() 
  lazy val uoaHelper = new UoaHelper()
  lazy val apis = new ApiService({() => idp}, CacheManager.getInstance().getCache("rais"))
  lazy val lafHelper = new Helper4Laf(new URI(S.contextPath + "/"), uoaHelper, idp)
  lazy val rawDataProvider : RawDataProvider = new RawDataProviderWithLocalFSCache(fsh, workdir, apis, uoaHelper)//new RawDataProvider0(workdir, apis, uoaHelper)
  lazy val idp : InfoDataProvider = new InfoDataProvider0(rawDataProvider, uoaHelper) with InfoDataProviderCache {
    protected val cacheUoa2info = CacheManager.getInstance().getCache("uoa2info")
    protected val cacheUoa2types = CacheManager.getInstance().getCache("uoa2types")
  }
  lazy val lafProvider = new LafProvider(workdir, lafHelper, idp, fsh)
  lazy val entityDisplayer: Box[EntityDisplayer] = lafProvider.newEntityDisplayer("entity0") //new EntityDisplayer4Debug()
  lazy val navigatorDisplayer: Box[NavigatorDisplayer] = lafProvider.newNavigatorDisplayer("navigator0") //new EntityDisplayer4Debug()
  lazy val sourceDisplayer : Box[SourceDisplayer] = lafProvider.newSourceDisplayer("src0")
  lazy val commentSystem : CommentSystem = new GCommentsService(new UrlMaker4GComments(uoaHelper), uoaHelper)

  def init() {
    apis.init()
    commentSystem.init()
  }
}