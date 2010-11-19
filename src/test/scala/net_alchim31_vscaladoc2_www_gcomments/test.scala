package net_alchim31_vscaladoc2_www_gcomments

import net.liftweb.common.Full
import net.liftweb.common.Empty


import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }
//import org.scalacheck.Gen

@RunWith(classOf[JUnitSuiteRunner])
class GCommentsFeedExtractorSpecTest extends Specification with JUnit /*with ScalaCheck*/ {
  
  import scala.xml.XML
  import org.joda.time.DateTime
  import net_alchim31_vscaladoc2_www.UoaHelper
  
  val urlMaker = new UrlMaker4GComments(new UoaHelper())
  val extractor = new GCommentsFeedExtractor(urlMaker)
  
  "GCommentsFeedExtractor" should {
    "select matching title" in {
      val pattern = urlMaker.ApiTitle.pattern.pattern
      "[API][Discuss] foo" must find(pattern).withGroups("foo")
      "[API][Discuss] artifactId/package/type"  must find(pattern).withGroups("artifactId/package/type")
      "[vscaladoc][API][Discuss] artifactId/package/type"  must find(pattern).withGroups("artifactId/package/type")
      "Re: [vscaladoc][API][Discuss] artifactId/package/type"  must find(pattern).withGroups("artifactId/package/type")
      "" must not(find(pattern))
      //0
    }
    "extract info from url" in {
      val pattern = urlMaker.UrlMessage.pattern.pattern
      "http://groups.google.com/group/vscaladoc/browse_thread/thread/7d7cfa9fb9ace2fd/a0e3bcb9a37cac13?show_docid=a0e3bcb9a37cac13" must find(pattern)
      "http://groups.google.com/group/vscaladoc/browse_thread/thread/7d7cfa9fb9ace2fd/3a8f30ff609d7e33?show_docid=3a8f30ff609d7e33" must find(pattern)
      //0
    }
    "read all valid entries " in {
      
      val data = XML.loadString("""
<?xml-stylesheet href="http://www.blogger.com/styles/atom.css" type="text/css"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <id>http://groups.google.com/group/vscaladoc</id>
  <title type="text">vscaladoc Google Group</title>
  <subtitle type="text">
  Talk about vscaladoc, the api and the tools
  </subtitle>
  <link title="vscaladoc feed" rel="self" href="/group/vscaladoc/feed/atom_v1_0_msgs.xml"></link>
  <updated>2010-11-18T23:07:59Z</updated>
  <generator version="1.99" uri="http://groups.google.com">Google Groups</generator>
  <entry>
  <author>
  <name>David Bernard</name>
  <email>david.bernard...@gmail.com</email>
  </author>
  <updated>2010-11-18T23:07:59Z</updated>
  <id>http://groups.google.com/group/vscaladoc/browse_thread/thread/b4ab97c6e4e5a76b/08e2bb5a863bc365?show_docid=08e2bb5a863bc365</id>
  <link href="http://groups.google.com/group/vscaladoc/browse_thread/thread/b4ab97c6e4e5a76b/08e2bb5a863bc365?show_docid=08e2bb5a863bc365"></link>
  <title type="text">Re: [vscaladoc] [API][Discuss] vscaladoc2_demoprj/_</title>
  <summary xml:space="preserve" type="html">
  reply to first message
  </summary>
  </entry>
  <entry>
  <author>
  <name>David Bernard</name>
  <email>david.bernard...@gmail.com</email>
  </author>
  <updated>2010-11-18T23:00:33Z</updated>
  <id>http://groups.google.com/group/vscaladoc/browse_thread/thread/b4ab97c6e4e5a76b/0cf2cd967d634951?show_docid=0cf2cd967d634951</id>
  <link href="http://groups.google.com/group/vscaladoc/browse_thread/thread/b4ab97c6e4e5a76b/0cf2cd967d634951?show_docid=0cf2cd967d634951"></link>
  <title type="text">[API][Discuss] vscaladoc2_demoprj/_</title>
  <summary xml:space="preserve" type="html">
  version : [0.1-SNAPSHOT](&lt;a target=&quot;_blank&quot; rel=nofollow href=&quot;http://vscaladoc.alchim31.net/laf/api//vscaladoc2_demoprj/0.1-SNAPSHOT&quot;&gt;[link]&lt;/a&gt;) &lt;br&gt; Second message to test discuss about artifact
  </summary>
  </entry>
  <entry>
  <author>
  <name>David Bernard</name>
  <email>dway...@free.fr</email>
  </author>
  <updated>2010-11-18T22:59:50Z</updated>
  <id>http://groups.google.com/group/vscaladoc/browse_thread/thread/b4ab97c6e4e5a76b/62819a2ec3d38749?show_docid=62819a2ec3d38749</id>
  <link href="http://groups.google.com/group/vscaladoc/browse_thread/thread/b4ab97c6e4e5a76b/62819a2ec3d38749?show_docid=62819a2ec3d38749"></link>
  <title type="text">[API][Discuss] vscaladoc2_demoprj/_</title>
  <summary xml:space="preserve" type="html">
  version : [0.1-SNAPSHOT](&lt;a target=&quot;_blank&quot; rel=nofollow href=&quot;http://vscaladoc.alchim31.net/laf/api//vscaladoc2_demoprj/0.1-SNAPSHOT&quot;&gt;[link]&lt;/a&gt;) &lt;br&gt; First message to test comment on artifact
  </summary>
  </entry>
  <entry>
  <author>
  <name>David Bernard</name>
  <email>david.bernard...@gmail.com</email>
  </author>
  <updated>2010-11-15T09:29:47Z</updated>
  <id>http://groups.google.com/group/vscaladoc/browse_thread/thread/7d7cfa9fb9ace2fd/a0e3bcb9a37cac13?show_docid=a0e3bcb9a37cac13</id>
  <link href="http://groups.google.com/group/vscaladoc/browse_thread/thread/7d7cfa9fb9ace2fd/a0e3bcb9a37cac13?show_docid=a0e3bcb9a37cac13"></link>
  <title type="text">Re: [vscaladoc] test0</title>
  <summary xml:space="preserve" type="html">
  reply
  </summary>
  </entry>
  <entry>
  <author>
  <name>David Bernard</name>
  <email>david.bernard...@gmail.com</email>
  </author>
  <updated>2010-11-14T12:58:20Z</updated>
  <id>http://groups.google.com/group/vscaladoc/browse_thread/thread/7d7cfa9fb9ace2fd/3a8f30ff609d7e33?show_docid=3a8f30ff609d7e33</id>
  <link href="http://groups.google.com/group/vscaladoc/browse_thread/thread/7d7cfa9fb9ace2fd/3a8f30ff609d7e33?show_docid=3a8f30ff609d7e33"></link>
  <title type="text">test0</title>
  <summary xml:space="preserve" type="html">
  test0
  </summary>
  </entry>

  <entry>
  <author>
  <name>David Bernard</name>
  <email>david.bernard...@gmail.com</email>
  </author>
  <updated>2010-11-15T09:29:47Z</updated>
  <id>http://groups.google.com/group/vscaladoc/browse_thread/thread/7d7cfa9fb9ace2fd/a0e3bcb9a37cac13?show_docid=a0e3bcb9a37cac13</id>
  <link href="http://groups.google.com/group/vscaladoc/browse_thread/thread/7d7cfa9fb9ace2fd/a0e3bcb9a37cac13?show_docid=a0e3bcb9a37cac13"/>
  <title type="text">Re: [vscaladoc][API][Discuss] artifactId/_/package/type</title>
  <summary type="html" xml:space="preserve">
  reply
  </summary>
  </entry>

  <entry>
  <author>
  <name>David Bernard</name>
  <email>david.bernard...@gmail.com</email>
  </author>
  <updated>2010-11-14T12:58:20Z</updated>
  <id>http://groups.google.com/group/vscaladoc/browse_thread/thread/7d7cfa9fb9ace2fd/3a8f30ff609d7e33?show_docid=3a8f30ff609d7e33</id>
  <link href="http://groups.google.com/group/vscaladoc/browse_thread/thread/7d7cfa9fb9ace2fd/3a8f30ff609d7e33?show_docid=3a8f30ff609d7e33"/>
  <title type="text">[API][Discuss] artifactId/_/package/type</title>
  <summary type="html" xml:space="preserve">
  test0
  </summary>
  </entry>
</feed>
      		""")
      val resNone = extractor.collectEntryInfoFromAtomFeed(data, new DateTime())
      resNone must haveSize(0)

      val resAll = extractor.collectEntryInfoFromAtomFeed(data, new DateTime(0))
      resAll must haveSize(5)

      val resAllFusion = extractor.fusionEntries(resAll, { _ => Empty}).toList
      resAllFusion must haveSize(2)
      resAllFusion(1).open_! must_== GCommentsInfo0("vscaladoc2_demoprj/_", 3, new DateTime("2010-11-18T22:59:50Z"), new DateTime("2010-11-18T23:07:59Z"), "vscaladoc", "b4ab97c6e4e5a76b", "62819a2ec3d38749")
      resAllFusion(0).open_! must_== GCommentsInfo0("artifactId/_/package/type", 2, new DateTime("2010-11-14T12:58:20Z"), new DateTime("2010-11-15T09:29:47Z"), "vscaladoc", "7d7cfa9fb9ace2fd", "3a8f30ff609d7e33")
      //0
    }
  }
}


object GCommentsFeedExtractorSpecMain {
  def main(args: Array[String]) {
    new GCommentsFeedExtractorSpecTest().main(args)
  }
}
