//#OPTIONS: CPP

/* platform-specific setup */

/*
   if browser mode is active (GHCJS_BROWSER is defined), all the runtime platform
   detection code should be removed by the preprocessor. The h$isPlatform variables
   are undeclared.

   in non-browser mode, use h$isNode, h$isJsShell, h$isBrowser to find the current
   platform.

   more platforms should be added here in the future
*/
#ifndef GHCJS_BROWSER
var h$isNode_    = false; // runtime is node.js
var h$isJvm_     = false; // runtime is JVM
var h$isJsShell_ = false; // runtime is SpiderMonkey jsshell
var h$isJsCore_  = false; // runtime is JavaScriptCore jsc
var h$isBrowser_ = false; // running in browser or everything else

var h$isGHCJSi_  = false; // Code is GHCJSi (browser or node)

function h$isNode() {
  return h$isNode_;
}

function h$isJvm() {
  return h$isJvm_;
}

function h$isJsShell() {
  return h$isJsShell_;
}

function h$isJsCore() {
  return h$isJsCore_;
}

function h$isBrowser() {
  return h$isBrowser_;
}

function h$isGHCJSi() {
  return h$isGHCJSi_;
}

// load all required node.js modules
if(typeof global !== 'undefined' && typeof process !== 'undefined' && (typeof h$TH !== 'undefined' || (typeof require !== 'undefined' && typeof module !== 'undefined' && module.exports))) {
    h$isNode_ = true;
    // we have to put fs var into closure to prevent name clashes with emscripten compiler code injections
    // we use additional suffix to have more guarantees over possible clashes
    // we should use amd/commonjs module wrappers to avoid such ugly things
    global.h$nodeOS_modules_qvsKGchAmE = (function() {
      var fs = require('fs');
      var path = require('path');
      var os = require('os');
      var child_process = require('child_process');
      return {
        'fs' : fs,
        'path': path,
        'os': os,
        'child_process': child_process
      };
    })();
    // we dictionary-like access strictly prevents
    // google closure compiler names mangling
    var h$fs          = global.h$nodeOS_modules_qvsKGchAmE['fs'];
    var h$path        = global.h$nodeOS_modules_qvsKGchAmE['path'];
    var h$os          = global.h$nodeOS_modules_qvsKGchAmE['os'];
    var h$child       = global.h$nodeOS_modules_qvsKGchAmE['child_process'];

    // we forcibly delete temporary lexical environment variable
    // to prevent any clashes
    delete global.h$nodeOS_modules_qvsKGchAmE;

    var h$process     = process;

    function h$getProcessConstants() {
      // this is a non-public API, but we need these values for things like file access modes
      var cs = process['binding']('constants');
      if(typeof cs.os === 'object' && typeof cs.fs === 'object') {
        return cs;
      } else {
        // earlier node.js versions (4.x and older) have all constants directly in the constants object
        // construct something that resembles the hierarchy of the object in new versions:
        return { 'fs':     cs
               , 'crypto': cs
               , 'os':     { 'UV_UDP_REUSEADDR': cs['UV_UDP_REUSEADDR']
                           , 'errno':            cs
                           , 'signals':          cs
                           }
               };
      }
    }
    var h$processConstants = h$getProcessConstants();
} else if(typeof Java !== 'undefined' && typeof java !== 'undefined') {
    h$isJvm_ = true;
    this.console = {
      log: function(s) {
        java.lang.System.out.print(s);
      }
    };
} else if(typeof snarf !== 'undefined' && typeof print !== 'undefined' && typeof quit !== 'undefined') {
    h$isJsShell_ = true;
    this.console = { log: this.print };
} else if(typeof numberOfDFGCompiles !== 'undefined' && typeof jscStack !== 'undefined') {
    h$isJsCore_ = true;
} else {
    h$isBrowser_ = true;
}
if(typeof global !== 'undefined' && global.h$GHCJSi) {
  h$isGHCJSi_ = true;
}
#endif

function h$getGlobal(that) {
    if(typeof global !== 'undefined') return global;
    return that;
}

#ifdef GHCJS_BROWSER
// IE 8 doesn't support Date.now(), shim it
if (!Date.now) {
  Date.now = function now() {
    return +(new Date);
  };
}
#endif
