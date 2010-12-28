(function() {
  var displayTypesFrag, updateClassesDisplay, updateFilter4Name, updateFilter4Packages;
  window.typeBrowserInfo = {
    filter4Packages: [],
    filter4NameRE: null,
    lastUpdateClassDisplayCallId: null,
    callCnt: 0,
    dataAll: [],
    templateNameOrig: null,
    containerSelectorOrig: null,
    displayData: [],
    displayOffset: 0
  };
  updateFilter4Packages = function() {
    var selected, _i, _len, _ref;
    window.typeBrowserInfo.filter4Packages = [];
    _ref = $("#packagesFilter > input:checked");
    for (_i = 0, _len = _ref.length; _i < _len; _i++) {
      selected = _ref[_i];
      window.typeBrowserInfo.filter4Packages.push(selected.value);
    }
    $("#packagesFilterSelectedCount").text("" + (window.typeBrowserInfo.filter4Packages.length > 0 ? window.typeBrowserInfo.filter4Packages.length : 'All') + " packages selected");
    return updateClassesDisplay(20);
  };
  updateFilter4Name = function() {
    var flags, ignoreCase, pattern, startsWith;
    pattern = $("#nameFilter").val();
    if ((pattern === null) || (pattern.length === 0)) {
      window.typeBrowserInfo.filter4NameRE = null;
    } else {
      flags = "";
      ignoreCase = $('input:radio[name=option_filter4NameMode]:checked').val() === "insensitive";
      startsWith = $('input:checkbox[name=option_filter4NameStartMode]:checked').val() === "startsWith";
      if (ignoreCase) {
        flags = "i";
        pattern = pattern.replace("\*", ".*?");
      } else {
        pattern = pattern.replace(/([A-Z][^A-Z]*)/g, "$1[^A-Z]*?") + ".*";
      }
      if (startsWith) {
        pattern = "^" + pattern;
      }
      console.log("filter4Name pattern : " + pattern);
      window.typeBrowserInfo.filter4NameRE = new RegExp(pattern, flags);
    }
    return updateClassesDisplay(150);
  };
  updateClassesDisplay = function(waitBefore) {
    var timestamp;
    if (window.typeBrowserInfo.lastUpdateClassDisplayCallId !== null) {
      clearTimeout(window.typeBrowserInfo.lastUpdateClassDisplayCallId);
    }
    waitBefore != null ? waitBefore : waitBefore = 300;
    timestamp = "" + (new Date).getTime();
    return window.typeBrowserInfo.lastUpdateClassDisplayCallId = setTimeout("updateClassesDisplayNow(" + (++window.typeBrowserInfo.callCnt) + ", " + timestamp + ")", waitBefore);
  };
  window.updateClassesDisplayNow = function(callId, requestAtTime) {
    var data, diff, filterEndAt, startAtTime;
    startAtTime = (new Date).getTime();
    data = jQuery.grep(window.typeBrowserInfo.dataAll, function(n, i) {
      return (callId === window.typeBrowserInfo.callCnt) && ((window.typeBrowserInfo.filter4Packages.length === 0) || (window.typeBrowserInfo.filter4Packages.indexOf(n.pkg) !== -1)) && ((window.typeBrowserInfo.filter4NameRE === null) || window.typeBrowserInfo.filter4NameRE.test(n.label));
    });
    filterEndAt = (new Date).getTime();
    console.log("nb data found :" + data.length + "/" + window.typeBrowserInfo.dataAll.length);
    if (callId !== window.typeBrowserInfo.callCnt) {
      return console.log("abort before update display");
    } else {
      displayTypes(data, null, null);
      diff = (new Date).getTime() - startAtTime;
      return console.log("updateClassesDisplayNow delay :" + (startAtTime - requestAtTime) + " ms, filter duration :" + (filterEndAt - startAtTime) + "ms, display duration :" + diff + " ms");
    }
  };
  displayTypesFrag = function() {
    var d;
    d = window.typeBrowserInfo.displayData.slice(window.typeBrowserInfo.displayOffset, window.typeBrowserInfo.displayOffset + 100);
    window.typeBrowserInfo.displayOffset += 100 + 1;
    $.tmpl(window.typeBrowserInfo.templateNameOrig, d).appendTo(window.typeBrowserInfo.containerSelectorOrig);
    if (window.typeBrowserInfo.displayOffset <= window.typeBrowserInfo.displayData.length) {
      return setTimeout(displayTypesFrag, 1);
    }
  };
  window.initDisplayTypes = function(data, templateName, containerSelector) {
    window.typeBrowserInfo.dataAll = data;
    window.typeBrowserInfo.templateNameOrig = templateName;
    return window.typeBrowserInfo.containerSelectorOrig = containerSelector;
  };
  window.displayTypes = function(data) {
    data != null ? data : data = window.typeBrowserInfo.dataAll;
    console.log("displayTypes :" + data.length);
    if (data !== [] && window.typeBrowserInfo.templateNameOrig !== null && window.typeBrowserInfo.containerSelectorOrig !== null) {
      $(window.typeBrowserInfo.containerSelectorOrig).empty();
      window.typeBrowserInfo.displayOffset = 0;
      window.typeBrowserInfo.displayData = data;
      return setTimeout(displayTypesFrag, 1);
    }
  };
  window.displayPackages = function(data) {
    var arr, select;
    data != null ? data : data = window.typeBrowserInfo.dataAll;
    $("#packagesFilter > input").unbind("change");
    arr = _.uniq(jQuery.map(data, function(n, i) {
      return n.pkg;
    }).sort(), true);
    select = $('#packagesFilter');
    $.each(arr, function(key, value) {
      return select.append("<input type='checkbox' value='" + value + "'/>" + value + "<br/>");
    });
    return $("#packagesFilter > input").change(updateFilter4Packages);
  };
  $(document).ready(function() {
    $("#nameFilter").val("");
    $("#nameFilter").bind("keyup", updateFilter4Name);
    $("#nameFilter").bind("keypress", function(ev) {
      var key;
      key = ev.keyCode || ev.charCode || 0;
      /* ENTER PRESSED*/;
      if (key === 10 || key === 13) {
        return $("ul#classes > li:first > a").each(function() {
          var fakeEvent;
          fakeEvent = document.createEvent("HTMLEvents");
          fakeEvent.initEvent("click", true, true);
          return this.dispatchEvent(fakeEvent);
        });
      }
    });
    $("#options_filter4Name > input").change(updateFilter4Name);
    return updateFilter4Packages();
  });
}).call(this);
