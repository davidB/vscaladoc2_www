package net_alchim31_vscaladoc2_www


import net.liftweb.common.{Full,Box,Empty,Failure,Loggable}

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
  lazy val apis = new ApiService({() => rdti})
  lazy val lafHelper = new Helper4Laf(new URI(S.contextPath + "/"), uoaHelper)
  lazy val rdti : InfoDataProvider = new InfoDataProvider0(new BasicRawDataProvider(fsh, workdir, apis), uoaHelper)
  lazy val lafProvider = new LafProvider(workdir, lafHelper, rdti, fsh)
  lazy val entityDisplayer: Box[EntityDisplayer] = lafProvider.newEntityDisplayer("entity0") //new EntityDisplayer4Debug()
  lazy val navigatorDisplayer: Box[NavigatorDisplayer] = lafProvider.newNavigatorDisplayer("navigator0") //new EntityDisplayer4Debug()

  def init() {
    apis.init()
  }
}