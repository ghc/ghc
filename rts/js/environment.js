//#OPTIONS: CPP

#ifdef GHCJS_TRACE_ENV
function h$logEnv() { h$log.apply(h$log,arguments); }
#define TRACE_ENV(args...) h$logEnv(args)
#else
#define TRACE_ENV(args...)
#endif

// set up debug logging for the current JS environment/engine
// browser also logs to <div id="output"> if jquery is detected
// the various debug tracing options use h$log
#ifndef GHCJS_BROWSER
var h$glbl;
function h$getGlbl() { h$glbl = this; }
h$getGlbl();
#endif
#ifdef GHCJS_LOG_BUFFER
var h$logBufferSize = 6000;
var h$logBufferShrink = 1000;
var h$logBuffer = [];
#endif
function h$log() {
#ifdef GHCJS_LOG_BUFFER
  if(!h$logBuffer) return;
  var s = '';
  for(var i=0;i<arguments.length;i++) { s = s + arguments[i]; }
  h$logBuffer.push(s);
  if(h$logBuffer.length > h$logBufferSize) h$logBuffer = h$logBuffer.slice(h$logBufferShrink);
#else
  try {
#ifndef GHCJS_BROWSER
    if(h$glbl) {
      if(h$glbl.console && h$glbl.console.log) {
        h$glbl.console.log.apply(h$glbl.console,arguments);
      } else {
        h$glbl.print.apply(this,arguments);
      }
    } else {
      if(typeof console !== 'undefined') {
#endif
        console.log.apply(console, arguments);
#ifndef GHCJS_BROWSER
      } else if(typeof print !== 'undefined') {
        print.apply(null, arguments);
      }
    }
#endif
  } catch(ex) {
    // ignore console.log exceptions (for example for IE9 when console is closed)
  }
#endif
#ifdef GHCJS_LOG_JQUERY
  // if we have jquery, add to <div id='output'> element
  if(typeof(jQuery) !== 'undefined') {
    var x = '';
    for(var i=0;i<arguments.length;i++) { x = x + arguments[i]; }
    var xd = jQuery("<div></div>");
    xd.text(x);
    jQuery('#output').append(xd);
  }
#endif
}

function h$collectProps(o) {
  var props = [];
  for(var p in o) { props.push(p); }
  return("{"+props.join(",")+"}");
}



// load the command line arguments in h$programArgs
// the first element is the program name
var h$programArgs_;
var h$rtsArgs_;

function h$programArgs() {
  if (!h$programArgs_) {
    h$initArgs();
  }
  return h$programArgs_;
}

function h$rtsArgs() {
  if (!h$rtsArgs_) {
    h$initArgs();
  }
  return h$rtsArgs_;
}

function h$initArgs() {
  #ifdef GHCJS_BROWSER
  h$programArgs_ = [ "a.js" ];
  #else
  if(h$isNode()) {
      h$programArgs_ = process.argv.slice(1);
  } else if(h$isJvm()) {
      h$programArgs_ = h$getGlobal(this).arguments.slice(0);
      h$programArgs_.unshift("a.js");
  } else if(h$isJsShell() && typeof h$getGlobal(this).scriptArgs !== 'undefined') {
      h$programArgs_ = h$getGlobal(this).scriptArgs.slice(0);
      h$programArgs_.unshift("a.js");
  } else if((h$isJsShell() || h$isJsCore()) && typeof h$getGlobal(this).arguments !== 'undefined') {
      h$programArgs_ = h$getGlobal(this).arguments.slice(0);
      h$programArgs_.unshift("a.js");
  } else {
      h$programArgs_ = [ "a.js" ];
  }
  #endif

  //filter RTS arguments
  {
      var prog_args = [];
      var rts_args = [];
      var in_rts = false;
      var i = 0;
      for(i=0;i<h$programArgs_.length;i++) {
          var a = h$programArgs_[i];
          // The '--RTS' argument disables all future
          // +RTS ... -RTS processing.
          if (a === "--RTS") {
              break;
          }
          // The '--' argument is passed through to the program, but
          // disables all further +RTS ... -RTS processing.
          else if (a === "--") {
              break;
          }
          else if (a === "+RTS") {
              in_rts = true;
          }
          else if (a === "-RTS") {
              in_rts = false;
          }
          else if (in_rts) {
              rts_args.push(a);
          }
          else {
              prog_args.push(a);
          }
      }
      // process remaining program arguments
      for (;i<h$programArgs_.length;i++) {
          prog_args.push(h$programArgs_[i]);
      }
      //set global variables
      h$programArgs_ = prog_args;
      h$rtsArgs_     = rts_args;
  }
}

function h$getProgArgv(argc_v,argc_off,argv_v,argv_off) {
  TRACE_ENV("getProgArgV")
  var c = h$programArgs().length;
  if(c === 0) {
    argc_v.dv.setInt32(argc_off, 0, true);
  } else {
    argc_v.dv.setInt32(argc_off, c, true);
    var argv = h$newByteArray(4*c);
    for(var i=0;i<h$programArgs().length;i++) {
      PUT_ADDR(argv,4*i,h$encodeUtf8(h$programArgs()[i]),0);
    }
    PUT_ADDR(argv_v,argv_off,argv,0);
  }
}

function h$setProgArgv(n, ptr_d, ptr_o) {
  var args = [];
  for(var i=0;i<n;i++) {
    var off = ptr_o+4*i;
    GET_ADDR(ptr_d,off,p,o);
    var arg = h$decodeUtf8z(p, o);
    args.push(arg);
  }
  h$programArgs_ = args;
}

function h$getpid() {
#ifndef GHCJS_BROWSER
  if(h$isNode()) return process.id;
#endif
  return 0;
}

function h$cpuTimePrecision() {
  return 1000;
}

var h$fakeCpuTime = 1.0;

function h$getCPUTime() {
#ifndef GHCJS_BROWSER
if(h$isNode()) {
  var t = process.cpuUsage();
  var cput = t.user + t.system;
  TRACE_ENV("getCPUTime: " + cput)
  return cput;
}
#endif
  // XXX this allows more testsuites to run
  //     but I don't really like returning a fake value here
  TRACE_ENV("getCPUTime: returning fake value")
  return ++h$fakeCpuTime;
  return -1;
}

function h$__hscore_environ() {
    TRACE_ENV("hscore_environ")
#ifndef GHCJS_BROWSER
    if(h$isNode()) {
        var env = [], i;
        for(i in process.env) {
          var envv = i + '=' + process.env[i];
          TRACE_ENV("hscore_environ: " + envv)
          env.push(envv);
        }
        if(env.length === 0) return null;
        var p = h$newByteArray(4*env.length+1);
        for(i=0;i<env.length;i++) {
          PUT_ADDR(p,4*i,h$encodeUtf8(env[i]),0);
        }
        PUT_ADDR(p,4*env.length,null,0);
        RETURN_UBX_TUP2(p, 0);
    }
#endif
    RETURN_UBX_TUP2(null, 0);
}

function h$__hsbase_unsetenv(name, name_off) {
    return h$unsetenv(name, name_off);
}

function h$getenv(name, name_off) {
    TRACE_ENV("getenv")
#ifndef GHCJS_BROWSER
    if(h$isNode()) {
        var n = h$decodeUtf8z(name, name_off);
        TRACE_ENV("getenv (node): " + n)
        if(typeof process.env[n] !== 'undefined') {
            TRACE_ENV("getenv (node): " + n + " -> " + process.env[n])
            RETURN_UBX_TUP2(h$encodeUtf8(process.env[n]), 0);
        }
    }
#endif
    RETURN_UBX_TUP2(null, 0);
}

function h$setenv(name, name_off, val, val_off, overwrite) {
  var n = h$decodeUtf8z(name, name_off);
  var v = h$decodeUtf8z(val, val_off);
  TRACE_ENV("setenv: " + n + " -> " + v)
  if(n.indexOf('=') !== -1) {
    h$setErrno("EINVAL");
    return -1;
  }
#ifndef GHCJS_BROWSER
  if(h$isNode()) {
    if(overwrite || typeof process.env[n] !== 'undefined') process.env[n] = v;
  }
#endif
  return 0;
}

function h$unsetenv(name, name_off) {
  var n = h$decodeUtf8z(name, name_off);
  TRACE_ENV("unsetenv: " + n)
  if(n.indexOf('=') !== -1) {
    h$setErrno("EINVAL");
    return -1;
  }
#ifndef GHCJS_BROWSER
  if(h$isNode()) delete process.env[n];
#endif
  return 0;
}

/*
  Note:
   SUSv2 specifies that the argument passed to putenv is made part
   of the environment. Later changes to the value will be reflected
   in the environment.

   this implementation makes a copy instead.
 */
function h$putenv(str, str_off) {
#ifndef GHCJS_BROWSER
  var x = h$decodeUtf8z(str, str_off);
  var i = x.indexOf('=');
  TRACE_ENV("putenv: " + x)
  if(i === -1) { // remove the value
    TRACE_ENV("putenv unset: " + x)
    if(h$isNode()) delete process.env[x];
  } else { // set the value
    var name = x.substring(0, i)
    var val = x.substring(i+1);
    TRACE_ENV("putenv set: " + name + " -> " + val)
    if(h$isNode()) process.env[name] = val;
  }
#endif
  return 0;
}

function h$errorBelch() {
  h$log("### errorBelch: do we need to handle a vararg function here?");
}

function h$errorBelch2(buf1, buf_offset1, buf2, buf_offset2) {
  var pat = h$decodeUtf8z(buf1, buf_offset1);
  h$errorMsg(h$append_prog_name(pat), h$decodeUtf8z(buf2, buf_offset2));
}

// append program name to the given string if possible
function h$append_prog_name(str) {
  // basename that only works with Unix paths for now...
  function basename(path) {
   return path.split('/').reverse()[0];
  }

  // only works for node for now
  if(h$isNode()) {
    return basename(process.argv[1]) + ": " + str;
  }

  return str;
}

function h$debugBelch2(buf1, buf_offset1, buf2, buf_offset2) {
  h$errorMsg(h$decodeUtf8z(buf1, buf_offset1), h$decodeUtf8z(buf2, buf_offset2));
}

function h$errorMsg(pat) {
#ifndef GHCJS_BROWSER
  function stripTrailingNewline(xs) {
    return xs.replace(/\r?\n$/, "");
  }
#endif
  // poor man's vprintf
  var str = pat;
  for(var i=1;i<arguments.length;i++) {
    str = str.replace(/%s/, arguments[i]);
  }
#ifndef GHCJS_BROWSER
  if(h$isGHCJSi()) {
    // ignore message
  } else if(h$isNode()) {
    process.stderr.write(str);
  } else if (h$isJsShell() && typeof printErr !== 'undefined') {
    if(str.length) printErr(stripTrailingNewline(str));
  } else if (h$isJsShell() && typeof putstr !== 'undefined') {
    putstr(str);
  } else if (h$isJsCore()) {
    if(str.length) {
	if(h$base_stderrLeftover.val !== null) {
	    debug(h$base_stderrLeftover.val + stripTrailingNewline(str));
	    h$base_stderrLeftover.val = null;
	} else {
	    debug(stripTrailingNewline(str));
	}
    }
  } else {
#endif
    if(typeof console !== 'undefined') {
      console.log(str);
    }
#ifndef GHCJS_BROWSER
  }
#endif
}

// this needs to be imported with foreign import ccall safe/interruptible
function h$performMajorGC() {
    // save current thread state so we can enter the GC
    var t = h$currentThread, err = null;
    t.sp = h$sp;
    h$currentThread = null;

    try {
        h$gc(t);
    } catch(e) {
        err = e;
    }

    // restore thread state
    h$currentThread = t;
    h$sp = t.sp;
    h$stack = t.stack;

    if(err) throw err;
}


function h$ghczminternalZCSystemziCPUTimeZCgetrusage() {
  return 0;
}

function h$getrusage() {
  return 0;
}


// fixme need to fix these struct locations

function h$gettimeofday(tv_v,tv_o,tz_v,tz_o) {
  var now = Date.now();
  tv_v.dv.setInt32(tv_o,     (now / 1000)|0, true);
  tv_v.dv.setInt32(tv_o + 4, ((now % 1000) * 1000)|0, true);
  if(tv_v.len >= tv_o + 12) {
    tv_v.dv.setInt32(tv_o + 8, ((now % 1000) * 1000)|0, true);
  }
  return 0;
}

var h$__hscore_gettimeofday = h$gettimeofday;

var h$myTimeZone = h$encodeUtf8("UTC");
function h$localtime_r(timep_v, timep_o, result_v, result_o) {
  var t = timep_v.i3[timep_o];
  var d = new Date(t * 1000);
  result_v.dv.setInt32(result_o     , d.getSeconds(), true);
  result_v.dv.setInt32(result_o + 4 , d.getMinutes(), true);
  result_v.dv.setInt32(result_o + 8 , d.getHours(), true);
  result_v.dv.setInt32(result_o + 12, d.getDate(), true);
  result_v.dv.setInt32(result_o + 16, d.getMonth(), true);
  result_v.dv.setInt32(result_o + 20, d.getFullYear()-1900, true);
  result_v.dv.setInt32(result_o + 24, d.getDay(), true);
  result_v.dv.setInt32(result_o + 28, 0, true); // fixme yday 1-365 (366?)
  result_v.dv.setInt32(result_o + 32, -1, true); // dst information unknown
  result_v.dv.setInt32(result_o + 40, 0, true); // gmtoff?
  PUT_ADDR(result_v,result_o+40, h$myTimeZone, 0);
  PUT_ADDR(result_v,result_o+48, h$myTimeZone, 0);
  RETURN_UBX_TUP2(result_v, result_o);
}
var h$__hscore_localtime_r = h$localtime_r;

function h$checkForeignRefs(refs) {
  function argSize(t) {
    if(t === "ghc-internal:GHC.Internal.Prim.Word64#")    return 2;
    if(t === "ghc-internal:GHC.Internal.Prim.State#")     return 0;
    if(t === "ghc-internal:GHC.Internal.Prim.Void#")      return 0;
    if(t === "ghc-internal:GHC.Internal.Prim.Int#")       return 1;
    if(t === "ghc-internal:GHC.Internal.Prim.Int64#")     return 2;
    if(t === "ghc-internal:GHC.Internal.Prim.Weak#")      return 1;
    if(t === "ghc-internal:GHC.Internal.Prim.Addr#")      return 2;
    if(t === "ghc-internal:GHC.Internal.Prim.Word#")      return 1;
    if(t === "ghc-internal:GHC.Internal.Prim.Float#")     return 1;
    if(t === "ghc-internal:GHC.Internal.Prim.Double#")    return 1;
    if(t === "ghc-internal:GHC.Internal.Prim.ByteArray#") return 2;
    if(t === "ghc-internal:GHC.Internal.Prim.ThreadId#")  return 1;
    console.warn("unknown argument type: " + t);
    return 1;
  }
  function callStr(r) {
    return r.pattern + '(' + r.arguments.join(', ') + ') -> ' + r.result + ' ' + r.span;
  }
  function checkRef(r) {
    if(r.cconv === "ccall") {
      var f = null;
      try {
        f = eval(r.pattern);
      } catch(e) { }
      if(!f) {
        console.warn("referenced pattern does not exist: " + callStr(r));
        return;
      }
      if(typeof f !== 'function') {
        console.warn("referenced pattern is not a function: " + callStr(r));
        return;
      }
      var s = 0, ba = 0;
      for(var i = 0; i < r.arguments.length; i++) {
        var a = r.arguments[i];
        s  += argSize(a);
        ba += a === "ghc-internal:GHC.Internal.Prim.ByteArray#" ? 1 : 0;
      }
      if(f.length != s) {
        console.warn("number of arguments does not seem to match: " + callStr(r));
      }
      if(ba !== 0 && f.length === (s - ba)) {
        console.warn("number of arguments matches old ByteArray calling convention: " + callStr(r));
      }
    }
    // todo: check other calling conventions
  }
  for(var i=0;i<refs.length;i++) {
    checkRef(refs[i]);
  }
}

var h$GHCConcSignalSignalHandlerStore_d = null;
var h$GHCConcSignalSignalHandlerStore_o = 0;

function h$getOrSetGHCConcSignalSignalHandlerStore(d,o) {
  if(d) {
    h$GHCConcSignalSignalHandlerStore_d = d;
    h$GHCConcSignalSignalHandlerStore_o = o;
  }
  RETURN_UBX_TUP2(h$GHCConcSignalSignalHandlerStore_d, h$GHCConcSignalSignalHandlerStore_o);
}
