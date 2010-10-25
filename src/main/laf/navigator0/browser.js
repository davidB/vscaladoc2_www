// manage user Options
var cfg = {
  filter4NameIgnoreCase : false,
  filter4NameAsRegExp : true
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
    updateClassesDisplay();
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
      var flags = (cfg.filter4NameIgnoreCase) ? "i": "";
      var pattern = (cfg.filter4NameAsRegExp) ? filter4Name : "^" + filter4Name;
      filter4NameRE = new RegExp(pattern, flags);
    }
    updateClassesDisplay();
};

var lastUpdateClassDisplayCallId = null;
var updateClassesDisplay = function() {
    if (lastUpdateClassDisplayCallId != null) {
        clearTimeout(lastUpdateClassDisplayCallId);
    }
    lastUpdateClassDisplayCallId = setTimeout("updateClassesDisplayNow("+ (new Date).getTime() +")", 300);
};
var updateClassesDisplayNow = function(requestAtTime) {
    var myUpdateClassDisplayCallId = lastUpdateClassDisplayCallId;
    var startAtTime = (new Date).getTime()
    var data = jQuery.grep( dataAll, function(n, i){
      return (myUpdateClassDisplayCallId == lastUpdateClassDisplayCallId)
        && ((filter4Packages.length == 0) || (jQuery.inArray(n.pkg, filter4Packages) != -1))
        && ((filter4NameRE == null) || filter4NameRE.test(n.label));
    });
    console.log("nb data found :" + data.length + "/"  + dataAll.length);
    if (myUpdateClassDisplayCallId != lastUpdateClassDisplayCallId) {
      console.log("abort before update display");
    } else {
      displayTypes(data, null, null);
      var diff = (new Date).getTime() - startAtTime;
      console.log("updateClassesDisplayNow delay :" + (startAtTime - requestAtTime) + " ms, duration :" + diff + " ms");
    }
};

var dataAll = [];
var templateOrig = null;
var containerSelector = null;

var displayTypes = function(data, templateName, containerSelector) {
  if (dataAll.length == 0) {
    dataAll = data;
    templateOrig = templateName;
    containerSelectorOrig = containerSelector;
  }
  if (templateName == null) {
    templateName = templateOrig;
  }
  if (containerSelector == null) {
    containerSelector = containerSelectorOrig;
  }
  if (data != [] && templateName != null && containerSelector != null) {
    $(containerSelector).empty();
    $.tmpl(templateName, data ).appendTo( containerSelector );
    $(containerSelector).fadeIn( "medium" );
  }
}

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
