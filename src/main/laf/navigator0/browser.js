//TODO clean and refactor (reduce number of non-function variable)
// manage user Options
/*
var cfg = {
  filter4NameIgnoreCase : false,
};


var togglefilter4NameOptions = function(optionName) {
  cfg[optionName] = !cfg[optionName];
  $.cookie(optionName, cfg[optionName]);
  $("input.option_" + optionName + "_cb").each(function(){this.checked = cfg[optionName]});
  updateFilter4NameRE();
};

$(document).ready(function(){
  for (optionName in cfg) {
    cfg[optionName] = $.cookie(optionName);
    cfg[optionName] = (cfg[optionName] == true || cfg[optionName] == "true");
    $("input.option_" + optionName + "_cb").each(function(){this.checked = cfg[optionName]});
  };
});
*/
// Filter

var filter4Packages = [];
var updateFilter4Packages = function(evt){
    filter4Packages = [];
    var select = $("#packagesFilter").get(0);//evt.target; //this
    for (var i=0; i<select.options.length; i++) {
        if (select.options[i].selected == true) {
            filter4Packages.push(select.options[i].text);
        }
    }
    updateClassesDisplay(20);
};

var filter4NameRE = null;
var filter4Name = "";

var updateFilter4Name = function(evt) {
    filter4Name = this.value;
    updateFilter4NameRE();
}
var updateFilter4NameRE = function() {
    if ((filter4Name == null) || (filter4Name.length == 0)) {
      filter4NameRE = null;
    } else {
      var flags = "";
      var pattern = filter4Name;
      var ignoreCase = $('input:radio[name=option_filter4NameMode]:checked').val() == "insensitive";
      console.log("mode : " + $('input:radio[name=option_filter4NameMode]:checked').val() + " .. " + ignoreCase);
      if (ignoreCase) {
        flags = "i";
        //support glob
        pattern = pattern.replace("\*", ".*?");
      } else {
        // support camelcase
        pattern = pattern.replace(/([A-Z][^A-Z]*)/g, "$1[^A-Z]*?") + ".*";
      }
      console.log("filter4Name pattern : " + pattern);
      filter4NameRE = new RegExp(pattern, flags);
    }
    updateClassesDisplay(150);
};

var lastUpdateClassDisplayCallId = null;
var callCnt = 0
var updateClassesDisplay = function(waitBefore) {
    if (lastUpdateClassDisplayCallId != null) {
        clearTimeout(lastUpdateClassDisplayCallId);
    }
    if (waitBefore == null) {
      waitBefore = 300
    }
    lastUpdateClassDisplayCallId = setTimeout("updateClassesDisplayNow("+ (++callCnt) + "," + (new Date).getTime() +")", waitBefore);
};
var updateClassesDisplayNow = function(callId, requestAtTime) {
    var startAtTime = (new Date).getTime()
    var data = jQuery.grep( dataAll, function(n, i){
      return (callId == callCnt)
        && ((filter4Packages.length == 0) || ( filter4Packages.indexOf(n.pkg) != -1))
        && ((filter4NameRE == null) || filter4NameRE.test(n.label));
    });
    var filterEndAt = (new Date).getTime();
    console.log("nb data found :" + data.length + "/"  + dataAll.length);
    if (callId != callCnt) {
      console.log("abort before update display");
    } else {
      displayTypes(data, null, null);
      var diff = (new Date).getTime() - startAtTime;
      console.log("updateClassesDisplayNow delay :" + (startAtTime - requestAtTime) + " ms, filter duration :" + (filterEndAt - startAtTime) + "ms, display duration :" + diff + " ms");
    }
};

var dataAll = [];
var templateNameOrig = null;
var containerSelectorOrig = null;

var initDisplayTypes = function(data, templateName, containerSelector) {
   dataAll = data;
   templateNameOrig = templateName
   containerSelectorOrig = containerSelector;
};

var displayTypes = function(data) {
  if (data == null) {
    data = dataAll;
  }
  if (data != [] && templateNameOrig != null && containerSelectorOrig != null) {
    $(containerSelectorOrig).empty();
    displayOffset = 0;
    displayData = data;
    // use a setTimeout instead of calling tmpl on every data in one shoot to avoid "stack space exhausted" on firefox
    setTimeout("displayTypesFrag()", 1);
//    $.tmpl(templateName, data ).appendTo( containerSelector );
//    $(containerSelector).fadeIn( "medium" );
  }
};

var displayData = [];
var displayOffset = 0;
var displayTypesFrag = function() {
  var d = displayData.slice(displayOffset, displayOffset+100);
  //console.log(" d :" + displayOffset + " // " + d.length + " //  " +displayData.length);
  displayOffset = displayOffset +101;
  $.tmpl(templateNameOrig, d).appendTo( containerSelectorOrig );
  if (displayOffset <= displayData.length) {
    setTimeout(displayTypesFrag, 1);
  }
};

$(document).ready(function(){
        $("#packagesFilter")
        .each(function() {
                for (var i=0; i<this.options.length; i++) {
                    this.options[i].selected = false;
                }
        })
        .bind("change", updateFilter4Packages)
        ;
        $("#nameFilter").val("");
        $("#nameFilter").bind("keyup", updateFilter4Name);
        $("input:radio[name=option_filter4NameMode]").bind("change", updateFilter4NameRE);

});

/**
* Selects an option by value
*
* @name     selectOptions
* @author   Mathias Bank (http://www.mathias-bank.de)
* @param    value specifies, which options should be selected
* @example  jQuery("#myselect").selectOptions("val1");
*
*/
jQuery.fn.selectOptions = function(value) {
    this.each(
        function()	{
            if(this.nodeName.toLowerCase() != "select") return;

            // get number of options
            var optionsLength = this.options.length;

            for(var i = 0; i<optionsLength; i++) {
                this.options[i].selected = (this.options[i].text == value);
            }
        }
    );
    return this;
};

var selectPackage = function(name) {
    $("#packagesFilter").selectOptions(name);
    updateFilter4Packages();
};
