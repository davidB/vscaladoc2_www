var showInherited = true;

var toggleInherited= function() {
  showInherited = !showInherited;
  $.cookie('showInherited', showInherited);
  updateInherited();
};

var updateInherited = function() {
  $("input.filter_inherited_cb").each(function(){this.checked = showInherited});
  if (showInherited) {
      $("tr.isInherited").show();
  } else {
      $("tr.isInherited").hide();
  }
};

//var logoMaxW=100;
//var logoMaxH=50;
//var imageResize = function(image) {
//  var h = image.height;
//  var w = image.width;
//  if ( h > logoMaxH ) {
//    w = Math.floor( w * logoMaxH / h );
//    h = logoMaxH;
//  }
//  if ( w > logoMaxW ) {
//    h = Math.floor( h * logoMaxW / w );
//    w = logoMaxW;
//  }
//  image.height = h;
//  image.width = w;
//};

var discussLoad = function() {
  var prefixLg = "discuss_".length; 
  var refPaths = $.makeArray($.map($(".discuss"), function(x){
    return x.id.substring(prefixLg);
  }));
  $.post("/comments.json",
    {'refPaths[]': refPaths},
    function(data){
      for (var key in data) {
        var value = data[key];
        var node = $(".discuss[id='discuss_" + key + "']");
        var nb = value[0];
        var url = value[1];
        if ( nb < 0) {
          node.removeClass('discuss').addClass('nodiscuss').text("X");
        } else if ( nb == 0){
          node.html("<a href='" + url + "'>" + nb + "</a>");
        } else {
          node.html("<a href='" + url + "' target='discussFrame' onclick='discussOpenFrame()'  title='Discuss'>" + nb + "</a>");
        }
      }
    },
    "json"
  );  
};
var discussOpenFrame = function() {
  var ctx = window.parent.document;
  var discussH = $('#discussFrame', ctx).attr("scrollHeight");
  var fullH0_3 = $('#contentFrameSet', ctx).attr("scrollHeight")*0.3;
  //console.log("discussH " + discussH + " ... "+ fullH0_3 + "..." + ((undefined != discussH) && (undefined != fullH0_3) && (discussH < fullH0_3)));
  
  //!!! BAD way to open the frame but don't find better (like dynamily change scrollHeight)
  // size always increase but when the value > to real size then no longer visual open
  if ((undefined != discussH) && (undefined != fullH0_3) && (discussH < fullH0_3)) {
    var current = $('#contentFrameSet', ctx).attr("rows").substring("*,".length).trim();
    //console.log("current " + current);
    var newDiscussH = parseInt(current) + (fullH0_3 - discussH);
    $("#contentFrameSet", window.parent.document).attr("rows", "*," + newDiscussH);
    //parent.document.getElementById("commentFrame").scrollHeight = parent.document.getElementById("commentFrame").scrollHeight + 10;
  }
};
var onReady = function(){
  parent.document.title=document.title;
  showInherited = $.cookie('showInherited');
  updateInherited();
  discussLoad();
  $("#logo * img").aeImageResize({ height: 70, width: 100 });
};
$(document).ready(onReady);

/**
 * Cookie plugin
 *
 * Copyright (c) 2006 Klaus Hartl (stilbuero.de)
 * Dual licensed under the MIT and GPL licenses:
 * http://www.opensource.org/licenses/mit-license.php
 * http://www.gnu.org/licenses/gpl.html
 *
 */

/**
 * Create a cookie with the given name and value and other optional parameters.
 *
 * @example $.cookie('the_cookie', 'the_value');
 * @desc Set the value of a cookie.
 * @example $.cookie('the_cookie', 'the_value', { expires: 7, path: '/', domain: 'jquery.com', secure: true });
 * @desc Create a cookie with all available options.
 * @example $.cookie('the_cookie', 'the_value');
 * @desc Create a session cookie.
 * @example $.cookie('the_cookie', null);
 * @desc Delete a cookie by passing null as value. Keep in mind that you have to use the same path and domain
 *       used when the cookie was set.
 *
 * @param String name The name of the cookie.
 * @param String value The value of the cookie.
 * @param Object options An object literal containing key/value pairs to provide optional cookie attributes.
 * @option Number|Date expires Either an integer specifying the expiration date from now on in days or a Date object.
 *                             If a negative value is specified (e.g. a date in the past), the cookie will be deleted.
 *                             If set to null or omitted, the cookie will be a session cookie and will not be retained
 *                             when the the browser exits.
 * @option String path The value of the path atribute of the cookie (default: path of page that created the cookie).
 * @option String domain The value of the domain attribute of the cookie (default: domain of page that created the cookie).
 * @option Boolean secure If true, the secure attribute of the cookie will be set and the cookie transmission will
 *                        require a secure protocol (like HTTPS).
 * @type undefined
 *
 * @name $.cookie
 * @cat Plugins/Cookie
 * @author Klaus Hartl/klaus.hartl@stilbuero.de
 */

/**
 * Get the value of a cookie with the given name.
 *
 * @example $.cookie('the_cookie');
 * @desc Get the value of a cookie.
 *
 * @param String name The name of the cookie.
 * @return The value of the cookie.
 * @type String
 *
 * @name $.cookie
 * @cat Plugins/Cookie
 * @author Klaus Hartl/klaus.hartl@stilbuero.de
 */
jQuery.cookie = function(name, value, options) {
    if (typeof value != 'undefined') { // name and value given, set cookie
        options = options || {};
        if (value === null) {
            value = '';
            options.expires = -1;
        }
        var expires = '';
        if (options.expires && (typeof options.expires == 'number' || options.expires.toUTCString)) {
            var date;
            if (typeof options.expires == 'number') {
                date = new Date();
                date.setTime(date.getTime() + (options.expires * 24 * 60 * 60 * 1000));
            } else {
                date = options.expires;
            }
            expires = '; expires=' + date.toUTCString(); // use expires attribute, max-age is not supported by IE
        }
        // CAUTION: Needed to parenthesize options.path and options.domain
        // in the following expressions, otherwise they evaluate to undefined
        // in the packed version for some reason...
        var path = options.path ? '; path=' + (options.path) : '';
        var domain = options.domain ? '; domain=' + (options.domain) : '';
        var secure = options.secure ? '; secure' : '';
        document.cookie = [name, '=', encodeURIComponent(value), expires, path, domain, secure].join('');
    } else { // only name given, get cookie
        var cookieValue = null;
        if (document.cookie && document.cookie != '') {
            var cookies = document.cookie.split(';');
            for (var i = 0; i < cookies.length; i++) {
                var cookie = jQuery.trim(cookies[i]);
                // Does this cookie string begin with the name we want?
                if (cookie.substring(0, name.length + 1) == (name + '=')) {
                    cookieValue = decodeURIComponent(cookie.substring(name.length + 1));
                    break;
                }
            }
        }
        return cookieValue;
    }
};


// COPY From https://github.com/adeelejaz/jquery-image-resize/blob/master/jquery.ae.image.resize.js
// Version: v2.1.0 (2010-11-20)
// Author: Adeel Ejaz (http://adeelejaz.com/)
// License: Dual licensed under MIT and GPL licenses.
(function( $ ) {

  $.fn.aeImageResize = function( params ) {

    // We cannot do much unless we have one of these
    if ( !params.height && !params.width ) {
      return this;
    }

    var mathFloor = Math.floor
      ,	isIE6 = $.browser.msie && (parseInt($.browser.version) == 6)
      ,	aspectRatio = 0
      ;

    // Calculate aspect ratio now, if possible
    if ( params.height && params.width ) {
      aspectRatio = params.width / params.height;
    }

    // Attach handler to load
    // Handler is executed just once per element
    // Load event required for Webkit browsers
    return this.one( "load", function() {

      // Remove all attributes and CSS rules
//      this
//        .removeAttr( "height" )
//        .removeAttr( "width" )
//        .css({
//          height: "",
//          width: ""
//        });

      var imgHeight = this.height
        ,	imgWidth = this.width
        ,	imgAspectRatio = imgWidth / imgHeight
        ,	bxHeight = params.height
        ,	bxWidth = params.width
        ,	bxAspectRatio = aspectRatio;

      // Work the magic!
      // If one parameter is missing, we just force calculate it
      if ( !bxAspectRatio ) {
        if ( bxHeight === 0 ) {
          bxAspectRatio = imgAspectRatio - 1;
        } else {
          bxAspectRatio = imgAspectRatio + 1;
        }
      }

      // Only resize the images that need resizing
      if ( (bxHeight && imgHeight > bxHeight) || (bxWidth && imgWidth > bxWidth) ) {

        if ( imgAspectRatio > aspectRatio ) {
          bxHeight = mathFloor( imgHeight / imgWidth * bxWidth );
        } else {
          bxWidth = mathFloor( imgWidth / imgHeight * bxHeight );
        }

        $( this ).attr({
          "height": bxHeight,
          "width": bxWidth
        });
      }
    })
    .each(function() {

      // Trigger load event (for Gecko and MSIE)
      if ( this.complete || isIE6 ) {
        this.trigger( "load" );
      }
    });
  };
})( jQuery );
