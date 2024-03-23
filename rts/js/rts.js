//#OPTIONS: CPP

var h$start = new Date();

function h$rts_eval(action, unbox) {
  return new Promise((accept, reject) =>
    h$run(MK_AP3( h$ghczminternalZCGHCziInternalziJSziPrimziresolveIO
                , x => { accept(unbox(x))}
                , e => { reject(new h$HaskellException(e))}
                , action
                ))
  );
}

function h$rts_eval_sync(closure, unbox) {
  var res, status = 0;
  try {
  h$runSync(MK_AP3( h$ghczminternalZCGHCziInternalziJSziPrimziresolveIO
           , x => { status = 1; res = unbox(x); }
           , e => { status = 2; res = new h$HaskellException(e); }
           , closure), false);
  } catch(e) { status = 2; res = e; }
  switch(status) {
    case 0:  throw new h$HaskellException("internal error"); // thread didn't reach update frame
    case 1:  return res;
    default: throw res;
  }
}

function h$rts_apply(f, x) {
  return MK_AP1(f, x);
}

/*
  marshalling for "foreign export"
 */
/*
   | getUnique tc `elem` [ intTyConKey, int8TyConKey, int16TyConKey
                         , int32TyConKey, int64TyConKey
                         , wordTyConKey, word8TyConKey, word16TyConKey
                         , word32TyConKey, word64TyConKey
                         , floatTyConKey, doubleTyConKey
                         , ptrTyConKey, funPtrTyConKey
                         , charTyConKey
                         , stablePtrTyConKey
                         , boolTyConKey
                         ]
  */

function h$rts_mkChar(x) { return x|0; }
function h$rts_getChar(x) { return UNWRAP_NUMBER(x); }

function h$rts_mkWord(x) { return x>>>0; }
function h$rts_getWord(x) { return UNWRAP_NUMBER(x); }

function h$rts_mkInt(x) { return x|0; }
function h$rts_getInt(x) { return UNWRAP_NUMBER(x); }

function h$rts_mkInt32(x) { return x|0; }
function h$rts_getInt32(x) { return UNWRAP_NUMBER(x); }

function h$rts_mkWord32(x) { return x>>>0; }
function h$rts_getWord32(x) { return UNWRAP_NUMBER(x); }

function h$rts_mkInt16(x) { return (x<<16)>>16; }
function h$rts_getInt16(x) { return UNWRAP_NUMBER(x); }

function h$rts_mkInt64(x) { throw new Error("rts_mkInt64"); /* return MK_INT64(); */ }
function h$rts_getInt64(x) { throw new Error("rts_getInt64"); }

function h$rts_mkWord64(x) { throw new Error("rts_mkWord64"); /* return MK_WORD64(); */ }
function h$rts_getWord64(x) { throw new Error("rts_getWord64"); }

function h$rts_mkWord16(x) { return x&0xffff; }
function h$rts_getWord16(x) { return UNWRAP_NUMBER(x); }

function h$rts_mkInt8(x) { return (x<<24)>>24; }
function h$rts_getInt8(x) { return UNWRAP_NUMBER(x); }

function h$rts_mkWord8(x) { return x&0xff; }
function h$rts_getWord8(x) { return UNWRAP_NUMBER(x); }

function h$rts_mkFloat(x) { return x; }
function h$rts_getFloat(x) { return x; }

function h$rts_mkDouble(x) { return x; }
function h$rts_getDouble(x) { return x; }

function h$rts_mkBool(x) { return x; }
function h$rts_getBool(x) { return x; }

function h$rts_getUnit(x) { return 0; }

function h$rts_toString(x) {
  var buf;
  if(typeof x === 'object' &&
     typeof x.len === 'number' &&
     x.buf instanceof ArrayBuffer) {
      buf = x;
  } else if(typeof x === 'object' &&
            x.buffer instanceof ArrayBuffer &&
            typeof x.byteOffset === 'number') {
    buf = h$wrapBuffer(x.buffer, true, x.byteOffset, x.byteLength);
  } else if(x instanceof ArrayBuffer) {
    buf = h$wrapBuffer(x, true, 0, x.byteLength);
  } else {
    throw new Error("rts_toString: unsupported value" + x);
  }
  return h$decodeUtf8z(buf);
}

function h$rts_mkPtr(x) {
  var buf, off = 0;
  // null pointer
  if(x === null) {
    buf = null;
    off = 0;
  }
  // Haskell pointer
  else if(typeof x == 'object' &&
     typeof x.offset == 'number' &&
     typeof x.array !== 'undefined') {
    buf = x.array;
    off = x.offset;
  }
  // JS string: UTF-8 encode
  else if(typeof x == 'string') {
    buf = h$encodeUtf8(x);
    off = 0;
  }
  // Haskell ByteArray
  else if(typeof x == 'object' &&
     typeof x.len == 'number' &&
     x.buf instanceof ArrayBuffer) {
    buf = x;
    off = 0;
  }
  // Offset in the Emscripten heap
  else if (typeof x == 'number' && h$HEAP !== null) {
    if (x == 0) {
      buf = null;
      off = 0;
    }
    else {
      buf = h$HEAP;
      off = x;
    }
  }
  // ArrayBufferView: make ByteArray with the same byteOffset
  else if(x.isView) {
    buf = h$wrapBuffer(x.buffer, true, 0, x.buffer.byteLength);
    off = x.byteOffset;
  }
  // plain ArrayBuffer
  else if (x instanceof ArrayBuffer) {
    buf = h$wrapBuffer(x, true, 0, x.byteLength);
    off = 0;
  }
  else {
    throw new Error ("h$rts_mkPtr: invalid argument: " + x);
  }
  return MK_PTR(buf, off);
}

function h$rts_getPtr(x) {
  var arr = x.d1;
  var offset = x.d2;
  return new Uint8Array(arr.buf, offset);
}

function h$rts_mkFunPtr(x) {
  // not yet implemented
  throw new Error("rts_mkFunPtr");
}

function h$rts_getFunPtr(x) {
  // not yet implemented
  throw new Error("rts_getFunPtr");
}

function h$rts_toIO(x) {
  return MK_AP1(h$ghczminternalZCGHCziInternalziJSziPrimzitoIO, x);
}

// running IO actions

function h$rts_evalIO_sync(closure) {
  // return h$runSyncReturn(closure, false);
}

async function h$rts_evalIO(closure) {

}

/*
function h$handleCallback(f, result) {
  try {
    f(result);
  } catch() {

  }
}
*/

/* end foreign export stuff */

function h$runio(c) {
  return h$c1(h$runio_e, c);
}

function h$runInitStatic() {
  if(h$initStatic.length == 0) return;
  for(var i=h$initStatic.length - 1;i>=0;i--) {
    h$initStatic[i]();
  }
  h$initStatic = [];
}

function h$o(o, typ, a, size, regs, srefs) {
  h$setObjInfo(o, typ, "", [], a, size, regs, srefs);
}

// set heap/stack object information
function h$setObjInfo(o, typ, name, fields, a, size, regs, srefs) {
  o.t    = typ;
  o.i    = fields;
  o.n    = name;
  o.a    = a;
  o.r    = regs;
  o.s    = srefs;
  o.m    = 0
  o.size = size;
}

var h$gccheckcnt = 0;

function h$gc_check(next) {
  //  h$log("gc_check: todo");
  if(++h$gccheckcnt > 1000) {
    for(var i=h$sp+1;i<h$stack.length;i++) {
      h$stack[i] = null;
    }
    h$gccheckcnt = 0;
  }
  return 0;
}

// print a closure
// fixme, property access here might be closure compiler incompatible

function h$printcl(i) {
  var cl = i.f;
  var d  = i.d1;
  var r  = "";
  switch(cl.t) {
    case h$ct_thunk:
    r += "thunk";
    break;
    case h$ct_con:
    r += "con[" + cl.a + "]";
    break;
    case h$ct_fun:
    r += "fun[" + cl.a + "]";
    break;
    default:
    r += "unknown closure type";
    break;
  }
  r += " :: " + cl.n + " ->";
  var idx = 1;
  // fixme update for single field data
  for(var i=0;i<cl.i.length;i++) {
    r += " ";
    switch(cl.i[i]) {
      case h$vt_ptr:
      r += "[ Ptr :: " + d["d"+idx] + "]";
      idx++;
      break;
      case h$vt_void:
      r += "void";
      break;
      case h$vt_double:
      r += "(" + d["d"+idx] + " :: double)";
      idx++;
      break;
      case h$vt_int:
      r += "(" + d["d"+idx] + " :: int)";
      idx++;
      break;
      case h$vt_long:
      r += "(" + d["d"+idx] + "," + d["d"+(idx+1)] + " :: long)";
      idx+=2;
      break;
      case h$vt_addr:
      r += "(" + d["d"+idx].length + "," + d["d"+(idx+1)] + " :: ptr)";
      idx+=2;
      break;
      default:
      r += "unknown field: " + cl.i[i];
    }
  }

}

function h$init_closure(c, xs) {
  c.m = 0;
  switch(xs.length) {
    case 0:
    c.d1 = null; c.d2 = null;
    return c;
    case 1:
    c.d1 = xs[0]; c.d2 = null;
    return c;
    case 2:
    c.d1 = xs[0]; c.d2 = xs[1];
    return c;
    case 3:
    c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2] };
    return c;
    case 4:
    c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3] };
    return c;
    case 5:
    c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3], d4: xs[4] };
    return c;
    case 6:
    c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3], d4: xs[4], d5: xs[5] };
    return c;
    case 7:
    c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3], d4: xs[4], d5: xs[5], d6: xs[6] };
    return c;
    default:
    c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3], d4: xs[4], d5: xs[5], d6: xs[6] };
    // fixme does closure compiler bite us here?
    for(var i=7;i<xs.length;i++) {
      c.d2["d"+i] = xs[i];
    }
    return c;
  }
}



function h$checkStack(f) {
  // some code doesn't write a stack frame header when called immediately
  if(f.t === h$ct_stackframe) h$stack[h$sp] = f;
  var idx = h$sp;
  while(idx >= 0) {
    f = h$stack[idx];
    var size, offset;
    if(typeof(f) === 'function') {
      if(f === h$ap_gen) {
        size = (h$stack[idx - 1] >> 8) + 2;
        offset = 2;
      } else {
        var tag = h$stack[idx].size;
        if(tag <= 0) {
          size = h$stack[idx-1];
          offset = 2;
        } else {
          size = (tag & 0xff) + 1;
          offset = 1;
        }
      }
      //      if(size < 1) throw("invalid stack frame size at: stack[" + idx + "], frame: " + h$stack[idx].n);
      //        h$log("checking frame: " + h$stack[idx].n + " size " + size);
      //      if(f !== h$returnf && f !== h$restoreThread) {
      //        for(var i=0;i<size-offset;i++) {
      //          if(typeof h$stack[idx-offset-i] === 'function') {
      //            h$dumpStackTop(h$stack, 0, h$sp);
      //            throw("unexpected function in frame at: " + idx + " " + h$stack[idx].n);
      //          }
      //        }
      //      }
      idx = idx - size;
    } else {
      h$dumpStackTop(h$stack, 0, h$sp);
      throw("invalid stack object at: " + idx);
    }
  }
}

function h$printReg(r) {
  if(r === null) {
    return "null";
  } else if(typeof r === 'object' && r.hasOwnProperty('f') && r.hasOwnProperty('d1') && r.hasOwnProperty('d2')) {
    if(typeof(r.f) !== 'function') {
      return "dodgy object";
    } else if(r.f.t === h$ct_blackhole && r.x) {
      return ("blackhole: -> " + h$printReg({ f: r.x.x1, d: r.d1.x2 }) + ")");
    } else {
      return ((r.alloc ? r.alloc + ': ' : '') + r.f.n + " (" + h$closureTypeName(r.f.t) + ", " + r.f.a + ")");
    }
  } else if(typeof r === 'object') {
    var res = h$collectProps(r);
    if(res.length > 40) {
      return (res.substr(0,40)+"...");
    } else {
      return res;
    }
  } else {
    var xs = new String(r) + "";
    if(xs.length > 40) {
      return xs.substr(0,40)+"...";
    } else {
      return xs;
    }
  }
}


function h$stackFrameSize(f) {
  if(f === h$ap_gen) { // h$ap_gen is special
    return (h$stack[h$sp - 1] >> 8) + 2;
  } else {
    var tag = f.size;
    if(tag < 0) {
      return h$stack[h$sp-1];
    } else {
      return (tag & 0xff) + 1;
    }
  }
}


// throw an exception: unwind the thread's stack until you find a handler
function h$throw(e, async) {
  //h$log("throwing exception: " + async);
  //h$dumpStackTop(h$stack, 0, h$sp);
  var origSp = h$sp;
  var lastBh = null; // position of last blackhole frame
  var f;
  while(h$sp > 0) {
    //h$log("unwinding frame: " + h$sp);
    f = h$stack[h$sp];
    if(f === null || f === undefined) {
      throw("h$throw: invalid object while unwinding stack");
    }
    if(f === h$catch_e) break;
    if(f === h$atomically_e) {
      if(async) { // async exceptions always propagate
        h$currentThread.transaction = null;
      } else if(!h$stmValidateTransaction()) { // restart transaction if invalid, don't propagate exception
        return h$stmStartTransaction(h$stack[h$sp]);
    }
  }
  if(f === h$catchStm_e && !async) break; // catchSTM only catches sync
  if(f === h$upd_frame) {
    var t = h$stack[h$sp-1];
    // wake up threads blocked on blackhole
    var waiters = t.d2;
    if(waiters !== null) {
      for(var i=0;i<waiters.length;i++) {
        h$wakeupThread(waiters[i]);
      }
    }
    if(async) {
      // convert blackhole back to thunk
      if(lastBh === null) {
        h$makeResumable(t,h$sp+1,origSp,[]); // [`R1`,h$return]);
      } else {
        h$makeResumable(t,h$sp+1,lastBh-2,[h$ap_0_0,h$stack[lastBh-1],h$return]);
      }
      lastBh = h$sp;
    } else {
      // just raise the exception in the thunk
      t.f = h$raise_e;
      t.d1 = e;
      t.d2 = null;
    }
  }
  var size = h$stackFrameSize(f);
  h$sp = h$sp - size;
}
//h$log("unwound stack to: " + `Sp`);
//h$dumpStackTop(`Stack`,0,origSp);
if(h$sp > 0) {
  var maskStatus = h$stack[h$p - 2];
  var handler = h$stack[h$sp - 1];
  if(f === h$catchStm_e) {
    h$currentThread.transaction = h$stack[h$sp-3];
    h$sp -= 4;
  } else if(h$sp > 3) { // don't pop the toplevel handler
  h$sp -= 3;
}
h$r1 = handler;
h$r2 = e;
if(f !== h$catchStm_e) {  // don't clobber mask in STM?
if(maskStatus === 0 && h$stack[h$sp] !== h$maskFrame && h$stack[h$sp] !== h$maskUnintFrame) {
  h$stack[h$sp+1] = h$unmaskFrame;
  h$sp += 1;
} else if(maskStatus === 1) {
  h$stack[h$sp+1] = h$maskUnintFrame;
  h$sp += 1;
}
h$currentThread.mask = 2;
}
return h$ap_2_1_fast();
} else {
  throw "unhandled exception in haskell thread";
}
}

// print top stack frame
function h$logStack() {
  if(typeof h$stack[h$sp] === 'undefined') {
    h$log("warning: invalid stack frame");
    return;
  }
  var size = 0;
  var gt = h$stack[h$sp].size;
  if(gt === -1) {
    size = h$stack[h$sp - 1] & 0xff;
  } else {
    size = gt & 0xff;
  }
  h$dumpStackTop(h$stack, h$sp-size-2, h$sp);
  for(var i=Math.max(0,h$sp-size+1); i <= h$sp; i++) {
    if(typeof h$stack[i] === 'undefined') {
      throw "undefined on stack";
    }
  }
}

// fixme check if still used
function h$ascii(s) {
  var res = [];
  for(var i=0;i<s.length;i++) {
    res.push(s.charCodeAt(i));
  }
  res.push(0);
  return res;
}


function h$dumpStackTop(stack, start, sp) {
  start = Math.max(start,0);
  for(var i=start;i<=sp;i++) {
    var s = stack[i];
    if(s && s.n) {
      h$log("stack[" + i + "] = " + s.n);
    } else {
      if(s === null) {
        h$log("stack[" + i + "] = null WARNING DANGER");
      } else if(typeof s === 'object' && s !== null && s.hasOwnProperty("f") && s.hasOwnProperty("d1") && s.hasOwnProperty("d2")) {
        if(typeof(s.f) !== 'function') {
          h$log("stack[" + i + "] = WARNING: dodgy object");
        } else {
          if(s.d1 === undefined) { h$log("WARNING: stack[" + i + "] d1 undefined"); }
          if(s.d2 === undefined) { h$log("WARNING: stack[" + i + "] d2 undefined"); }
          if(s.f.t === h$ct_blackhole && s.d1 && s.d1.x1 && s.d1.x1.n) {
            h$log("stack[" + i + "] = blackhole -> " + s.d1.x1.n);
          } else {
            h$log("stack[" + i + "] = -> " + (s.alloc ? s.alloc + ': ' : '') + s.f.n + " (" + h$closureTypeName(s.f.t) + ", a: " + s.f.a + ")");
          }
        }
      } else if(h$isInstanceOf(s,h$MVar)) {
        var val = s.val ===
        null ? " empty"
        : " value -> " + (typeof s.val === 'object' ? s.val.f.n + " (" + h$closureTypeName(s.val.f.t) + ", a: " + s.val.f.a + ")" : s.val);
        h$log("stack[" + i + "] = MVar " + val);
      } else if(h$isInstanceOf(s,h$MutVar)) {
        h$log("stack[" + i + "] = IORef -> " + (typeof s.val === 'object' ? (s.val.f.n + " (" + h$closureTypeName(s.val.f.t) + ", a: " + s.val.f.a + ")") : s.val));
      } else if(Array.isArray(s)) {
        h$log("stack[" + i + "] = " + ("[" + s.join(",") + "]").substring(0,50));
      } else if(typeof s === 'object') {
        h$log("stack[" + i + "] = " + h$collectProps(s).substring(0,50));
      } else if(typeof s === 'function') {
        var re = new RegExp("([^\\n]+)\\n(.|\\n)*");
        h$log("stack[" + i + "] = " + (""+s).substring(0,50).replace(re,"$1"));
      } else {
        h$log("stack[" + i + "] = " + (""+s).substring(0,50));
      }
    }
  }
}


/* check that a haskell heap object is what we expect:
f is a haskell entry function
d exists, but might be null, if it isn't, warn for any undefined/null fields or fields with unfamiliar names
*/
function h$checkObj(obj) {
  if(typeof obj === 'boolean' || typeof obj === 'number') { return; }
  if(!obj.hasOwnProperty("f") ||
  obj.f === null ||
  obj.f === undefined ||
  obj.f.a === undefined ||
  typeof obj.f !== 'function') {
    h$log("h$checkObj: WARNING, something wrong with f:");
    h$log((""+obj).substring(0,200));
    h$log(h$collectProps(obj));
    h$log(typeof obj.f);
  }
  if(!obj.hasOwnProperty("d1") || obj.d1 === undefined) {
    h$log("h$checkObj: WARNING, something wrong with d1:");
    h$log((""+obj).substring(0,200));
  } else if(!obj.hasOwnProperty("d2") || obj.d2 === undefined) {
    h$log("h$checkObj: WARNING, something wrong with d2:");
    h$log((""+obj).substring(0,200));
  } else if(obj.d2 !== null && typeof obj.d2 === 'object' && obj.f.size !== 2) {
    var d = obj.d2;
    for(var p in d) {
      if(d.hasOwnProperty(p)) {
        if(p.substring(0,1) != "d") {
          h$log("h$checkObj: WARNING, unexpected field name: " + p);
          h$log((""+obj).substring(0,200));
        }
        if(d[p] === undefined) {
          h$log("h$checkObj: WARNING, undefined field detected: " + p);
          h$log((""+obj).substring(0,200));
        }
        //        if(d[p] === null) {
        //          h$log("h$checkObj: WARNING, null field detected: " + p);
        //          h$log((""+obj).substring(0,200));
        //        }
      }
    }
    switch(obj.f.size) {
      case 6: if(d.d5 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d5"); }
      case 5: if(d.d4 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d4"); }
      case 4: if(d.d3 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d3"); }
      case 3: if(d.d2 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d2"); }
      if(d.d1 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d1"); }
      default: d = obj.d2; // dummy
    }
  }
}

function h$traceForeign(f, as) {
  if(!h$rts_traceForeign) { return; }
  var bs = [];
  for(var i=0;i<as.length;i++) {
    var ai = as[i];
    if(ai === null) {
      bs.push("null");
    } else if(typeof ai === 'object') {
      var astr = ai.toString();
      if(astr.length > 40) {
        bs.push(astr.substring(0,40)+"...");
      } else {
        bs.push(astr);
      }
    } else {
      bs.push(""+ai);
    }
  }
  h$log("ffi: " + f + "(" + bs.join(",") + ")");
}

function h$papArity(cp) {
  return cp.d2.d1;
}

// carefully suspend the current thread, looking at the
// function that would be called next
function h$suspendCurrentThread(next) {
  // `assertRts s (next |!== (TxtI "h$reschedule")) ("suspend called with h$reschedule"::String)`;
  if(next === h$reschedule) { throw "suspend called with h$reschedule"; }
  // some stack calls do not write the function to the stack top as an optimization
  // do it here
  if(next.t === h$ct_stackframe) h$stack[h$sp] = next;
  if(h$stack[h$sp] === h$restoreThread || next === h$return) {
    h$currentThread.sp = h$sp;
    return;
  }
  var nregs;
  var skipregs = 0;
  var t = next.t;
  // pap arity
  if(t === h$ct_pap) {
    nregs = (h$papArity(h$r1) >> 8) + 1;
  } else if(t === h$ct_fun || t === h$ct_stackframe) {
    // for normal functions, the number active registers is in the .r proprty
    nregs    = next.r >> 8;
    skipregs = next.r & 0xff;
  } else {
    nregs = 1;  // Thunk, Con, Blackhole only have R1
  }
  // h$log("suspending: " + `Sp` + " nregs: " + nregs);
  h$sp = h$sp+nregs+skipregs+3;
  var i;
  for(i=1;i<=skipregs;i++) {
    h$stack[h$sp-2-i] = null;
  }
  for(i=skipregs+1;i<=nregs+skipregs;i++) {
    h$stack[h$sp-2-i] = h$getReg(i);
  }
  h$stack[h$sp-2] = next;
  h$stack[h$sp-1] = nregs+skipregs+3;
  h$stack[h$sp]   = h$restoreThread;
  h$currentThread.sp = h$sp;
}

function h$static_thunk(f) {
  // fixme push stuff to restore stuff here
  var h;
  if(!h$rts_profiling) {
    h = { f: f, d1: null, d2: null, m: 0 };
  } else {
    h = { f: f, d1: null, d2: null, m: 0, cc: h$CCS_SYSTEM };
  }
  h$CAFs.push(h);
  h$CAFsReset.push(f);
  return h;
}

function h$catch(a, handler) {
  h$sp += 3;
  h$stack[h$sp-2] = h$currentThread.mask;
  h$stack[h$sp-1] = handler;
  h$stack[h$sp] = h$catch_e;
  h$r1 = a;
  return h$ap_1_0_fast();
}

function h$keepAlive(x, f) {
  h$sp += 2;
  h$stack[h$sp-1] = x;
  h$stack[h$sp] = h$keepAlive_e;
  h$r1 = f;
  return h$ap_1_0_fast();
}

// It is required by Google Closure Compiler to be at least defined if
// somewhere it is used
var h$libdwLookupLocation, h$libdwPoolRelease, h$libdwPoolTake,
  h$libdwGetBacktrace, h$backtraceFree
h$libdwLookupLocation
  = h$libdwPoolRelease
  = h$libdwPoolTake
  = h$libdwGetBacktrace
  = h$backtraceFree
  = function() {
    throw new Error('Libdw: Not Implemented Yet')
  }

// It is required by Google Closure Compiler to be at least defined if
// somewhere it is used
var h$lookupIPE
h$lookupIPE
  = function () {
    throw new Error('IPE: Not Implemented Yet')
  }