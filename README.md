SEE [vscaladoc2_genjson](../vscaladoc2_genjson/README.md) for global info about the vscaladoc2 family projects.


## TODO, ideas,.... :

* add user support
  * registration of user
  * registration of artifact
  * upload of skin
  * upload of css for selected skin
* add user preferences
  * default navigator
  * default skin to use
  * default css to use
  * display inherited
  * favorite artifact
* add page to search
  * artifact
  * Type
    * by name
    * by signature
  * method
    * by name
    * by signature
* improve default skin
* improve performance
  * cache rendering result
  * cache result of remote json request
  * cache in memory json info representation
  * cache entity relation
  * cache member (field,
* add support to upload skin(LaF)
* add support to test skin without server
  * provide a json sample
  * create a stand-alone local generator to convert json into static html site (like before).
* add support for custom navigator (uploadable like skin)
* integrate a user comment support
  * disquss
  * collabdoc
  * ...
* provide a chat (one room per artifact)

---

inspiraton : jdocs

http://www.jdocs.com/httpclient/3.0.1/api-index.html?m=class&p=org.apache.commons.httpclient&c=ChunkedOutputStream&render=classic

---
json format
link classic :
  http://.....
link to api :
  api:/artifactId/version/packageName/typeName/memberName/memberType64
  api:/artifactId/version/packageName/typeName/memberName
  api:/artifactId/version/packageName/typeName
  api:/artifactId/version/packageName
link to source :
  src:/artifactId/version/filepath_relative_to sourcedir/linenumber/columnnumberbegin/columnnumberend
  src:/artifactId/version/filepath_relative_to sourcedir/linenumber/columnnumberbegin
  src:/artifactId/version/filepath_relative_to sourcedir/linenumber
  src:/artifactId/version/filepath_relative_to sourcedir/linenumber
link to test :
  via link to src
