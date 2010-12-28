# Filter
window.typeBrowserInfo =
  filter4Packages : []
  filter4NameRE : null
  lastUpdateClassDisplayCallId : null
  callCnt : 0
  dataAll : []
  templateNameOrig : null
  containerSelectorOrig : null
  displayData : []
  displayOffset : 0
  
updateFilter4Packages = () ->
  window.typeBrowserInfo.filter4Packages = []
  window.typeBrowserInfo.filter4Packages.push selected.value for selected in $("#packagesFilter > input:checked") #evt.target
  $("#packagesFilterSelectedCount").text("#{if window.typeBrowserInfo.filter4Packages.length > 0 then window.typeBrowserInfo.filter4Packages.length else 'All'} packages selected")

  #console.log(window.typeBrowserInfo.filter4Package)  
  updateClassesDisplay(20)

updateFilter4Name = () ->
  pattern = $("#nameFilter").val()
  if ((pattern == null) || (pattern.length == 0))
    window.typeBrowserInfo.filter4NameRE = null
  else
    flags = ""
    ignoreCase = $('input:radio[name=option_filter4NameMode]:checked').val() == "insensitive"
    startsWith = $('input:checkbox[name=option_filter4NameStartMode]:checked').val() == "startsWith"
    #console.log("startsWith : "+ $('input:checkbox[name=option_filter4NameStartMode]:checked').val())
    #console.log("mode : " + $('input:radio[name=option_filter4NameMode]:checked').val() + " .. " + ignoreCase)
    if (ignoreCase)
      flags = "i"
      #support glob
      pattern = pattern.replace("\*", ".*?")
    else
      # support camelcase
      pattern = pattern.replace(/([A-Z][^A-Z]*)/g, "$1[^A-Z]*?") + ".*"
    pattern = "^" + pattern if startsWith
    console.log("filter4Name pattern : " + pattern)
    window.typeBrowserInfo.filter4NameRE = new RegExp(pattern, flags)
  updateClassesDisplay(150)

updateClassesDisplay = (waitBefore) ->
  clearTimeout(window.typeBrowserInfo.lastUpdateClassDisplayCallId) if (window.typeBrowserInfo.lastUpdateClassDisplayCallId != null)
  waitBefore ?= 300
  timestamp = "" + (new Date).getTime()
  window.typeBrowserInfo.lastUpdateClassDisplayCallId = setTimeout("updateClassesDisplayNow(#{++window.typeBrowserInfo.callCnt}, #{timestamp})", waitBefore)

window.updateClassesDisplayNow = (callId, requestAtTime) ->
  startAtTime = (new Date).getTime()
  data = jQuery.grep( window.typeBrowserInfo.dataAll, (n, i) ->
    (callId == window.typeBrowserInfo.callCnt) &&
     ((window.typeBrowserInfo.filter4Packages.length == 0) || ( window.typeBrowserInfo.filter4Packages.indexOf(n.pkg) != -1)) &&
     ((window.typeBrowserInfo.filter4NameRE == null) || window.typeBrowserInfo.filter4NameRE.test(n.label));
  )
  filterEndAt = (new Date).getTime()
  console.log("nb data found :" + data.length + "/"  + window.typeBrowserInfo.dataAll.length)
  if (callId != window.typeBrowserInfo.callCnt)
    console.log("abort before update display")
  else
    displayTypes(data, null, null)
    diff = (new Date).getTime() - startAtTime
    console.log("updateClassesDisplayNow delay :" + (startAtTime - requestAtTime) + " ms, filter duration :" + (filterEndAt - startAtTime) + "ms, display duration :" + diff + " ms")


displayTypesFrag = () ->
  d = window.typeBrowserInfo.displayData.slice(window.typeBrowserInfo.displayOffset, window.typeBrowserInfo.displayOffset+100)
  #console.log("displayTypesFrag d :" + window.typeBrowserInfo.displayOffset + " // " + d.length + " //  " + window.typeBrowserInfo.displayData.length);
  window.typeBrowserInfo.displayOffset += 100 + 1
  $.tmpl(window.typeBrowserInfo.templateNameOrig, d).appendTo( window.typeBrowserInfo.containerSelectorOrig )
  setTimeout(displayTypesFrag, 1) if (window.typeBrowserInfo.displayOffset <= window.typeBrowserInfo.displayData.length)

window.initDisplayTypes = (data, templateName, containerSelector) ->
 window.typeBrowserInfo.dataAll = data
 window.typeBrowserInfo.templateNameOrig = templateName
 window.typeBrowserInfo.containerSelectorOrig = containerSelector

window.displayTypes = (data) -> 
  data ?= window.typeBrowserInfo.dataAll
  console.log("displayTypes :" + data.length)
  if (data != [] && window.typeBrowserInfo.templateNameOrig != null && window.typeBrowserInfo.containerSelectorOrig != null)
    $(window.typeBrowserInfo.containerSelectorOrig).empty()
    window.typeBrowserInfo.displayOffset = 0
    window.typeBrowserInfo.displayData = data
    # use a setTimeout instead of calling tmpl on every data in one shoot to avoid "stack space exhausted" on firefox
    setTimeout(displayTypesFrag, 1)
#    $.tmpl(templateName, data ).appendTo( containerSelector )
#    $(containerSelector).fadeIn( "medium" )

window.displayPackages = (data) ->
  data ?= window.typeBrowserInfo.dataAll  
  $("#packagesFilter > input").unbind("change")
  arr = _.uniq(jQuery.map(data, (n, i)-> n.pkg).sort(), true)
  select = $('#packagesFilter')
  #$.each(arr, function(key, value) {
  #  select.append($('<option></option>').val(value).html(value));
  #});
  $.each(arr, (key, value) ->
    select.append("<input type='checkbox' value='#{value}'/>#{value}<br/>")
  )
  $("#packagesFilter > input").change(updateFilter4Packages)

$(document).ready(() ->
  #$("#packagesFilter").each(() ->
  #  option.selected = false for option in @options
  #).change(updateFilter4Packages)
  $("#nameFilter").val("")
  $("#nameFilter").bind("keyup", updateFilter4Name)
  $("#nameFilter").bind("keypress", (ev) ->
    key = ev.keyCode || ev.charCode || 0
    /* ENTER PRESSED*/
    if key == 10 || key == 13
      $("ul#classes > li:first > a").each(() ->
        #.trigger('click') doesn't simulate a click on a link
        # alternative something like top.frames[this.target].location = this.href
        fakeEvent = document.createEvent("HTMLEvents")
        fakeEvent.initEvent("click", true, true)
        this.dispatchEvent(fakeEvent)
      )
  )
  $("#options_filter4Name > input").change(updateFilter4Name)
  updateFilter4Packages()
)

