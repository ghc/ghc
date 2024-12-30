//#OPTIONS: CPP

// Used definitions: PROFILING, GHCJS_BROWSER and GHCJS_ASSERT_PROF

#ifdef GHCJS_ASSERT_PROF
function assert(condition, message) {
    if (!condition) {
        console.trace(message || "Assertion failed");
    }
}
#define ASSERT(args...) { assert(args); }
#else
#define ASSERT(args...)
#endif

#ifdef PROFILING
#define TRACE(args...) { h$log(args); }
#else
#define TRACE(args...)
#endif

/*
  install the ghcjs-profiling package from /utils/ghcjs-node-profiling
  to collect cost centre stack information with the node.js profiler
 */
var h$registerCC = null, h$registerCCS = null, h$setCCS = null;
var h$runProf = function(f) {
    f();
}

#ifndef GHCJS_BROWSER
if(h$isNode()) {
    (function() {
	try {
            var p = require('ghcjs-profiling');
            if(p.isProfiling()) {
		h$registerCC  = p.registerCC;
		h$registerCCS = p.registerCCS;
		h$setCCS      = p.setCCS;
		h$runProf     = p.runCC;
            }
	} catch(e) {}
    })();
}
#endif

var h$cachedCurrentCcs = -1;
function h$reportCurrentCcs() {
    if(h$setCCS) {
        if(h$currentThread) {
            var ccsKey = h$currentThread.ccs._key;
            if(h$cachedCurrentCcs !== ccsKey) {
                h$cachedCurrentCcs = ccsKey;
                h$setCCS(ccsKey);
            }
        } else if(h$cachedCurrentCcs !== -1) {
            h$cachedCurrentCcs = -1;
            h$setCCS(2147483647); // set to invalid CCS
        }
    }
}


var h$ccList  = [];
var h$ccsList = [];

var h$CCUnique = 0;
/** @constructor */
function h$CC(label, module, srcloc, isCaf) {
  //TRACE("h$CC(", label, ", ", module, ", ", srcloc, ", ", isCaf, ")")
  this.label     = label;
  this.module    = module;
  this.srcloc    = srcloc;
  this.isCaf     = isCaf;
  this._key      = h$CCUnique++;
  this.memAlloc  = 0;
  this.timeTicks = 0;
  if(h$registerCC) h$registerCC(this._key, label, module + ' ' + srcloc, -1,-1);
  h$ccList.push(this);
}


var h$CCSUnique = 0;
/** @constructor */
function h$CCS(parent, cc) {
  //TRACE("h$mkCCS(", parent, cc, ")")
  if (parent !== null && parent.consed.has(cc)) {
    return (parent.consed.get(cc));
  }
  this.consed = new h$Map();
  this.cc     = cc;
  this._key   = h$CCSUnique++;
  if (parent) {
    this.root      = parent.root;
    this.depth     = parent.depth + 1;
    this.prevStack = parent;
    parent.consed.put(cc,this);
  } else {
    this.root      = this;
    this.depth     = 0;
    this.prevStack = null;
  }
  this.prevStack      = parent;
  this.sccCount       = 0;
  this.timeTicks      = 0;
  this.memAlloc       = 0;
  this.inheritedTicks = 0;
  this.inheritedAlloc = 0;
  if(h$registerCCS) {
    var x = this, stack = [];
    while(x) { stack.push(x.cc._key); x = x.prevStack; }
    h$registerCCS(this._key, stack);
  }
  h$ccsList.push(this);  /* we need all ccs for statistics, not just the root ones */
}


//
// Built-in cost-centres and stacks
//

var h$CC_MAIN       = new h$CC("MAIN", "MAIN", "<built-in>", false);
var h$CC_SYSTEM     = new h$CC("SYSTEM", "SYSTEM", "<built-in>", false);
var h$CC_GC         = new h$CC("GC", "GC", "<built-in>", false);
var h$CC_OVERHEAD   = new h$CC("OVERHEAD_of", "PROFILING", "<built-in>", false);
var h$CC_DONT_CARE  = new h$CC("DONT_CARE", "MAIN", "<built-in>", false);
var h$CC_PINNED     = new h$CC("PINNED", "SYSTEM", "<built-in>", false);
var h$CC_IDLE       = new h$CC("IDLE", "IDLE", "<built-in>", false);
var h$CAF_cc        = new h$CC("CAF", "CAF", "<built-in>", false);

var h$CCS_MAIN      = new h$CCS(null, h$CC_MAIN);

var h$CCS_SYSTEM    = new h$CCS(h$CCS_MAIN, h$CC_SYSTEM);
var h$CCS_GC        = new h$CCS(h$CCS_MAIN, h$CC_GC);
var h$CCS_OVERHEAD  = new h$CCS(h$CCS_MAIN, h$CC_OVERHEAD);
var h$CCS_DONT_CARE = new h$CCS(h$CCS_MAIN, h$CC_DONT_CARE);
var h$CCS_PINNED    = new h$CCS(h$CCS_MAIN, h$CC_PINNED);
var h$CCS_IDLE      = new h$CCS(h$CCS_MAIN, h$CC_IDLE);
var h$CAF           = new h$CCS(h$CCS_MAIN, h$CAF_cc);


//
// Cost-centre entries, SCC
//

#ifdef PROFILING
function h$ccsString(ccs) {
  var labels = [];
  do {
    labels.push(ccs.cc.module+'.'+ccs.cc.label+' '+ccs.cc.srcloc);
    ccs = ccs.prevStack;
  } while (ccs !== null);
  return '[' + labels.reverse().join(', ') + ']';
}
#endif

#ifdef PROFILING
function h$pushRestoreCCS() {
    TRACE("push restoreccs:" + h$ccsString(h$currentThread.ccs))
    if(h$stack[h$sp] !== h$setCcs_e) {
        h$sp += 2;
        h$stack[h$sp-1] = h$currentThread.ccs;
        h$stack[h$sp]   = h$setCcs_e;
    }
}
#endif

function h$restoreCCS(ccs) {
    TRACE("restoreccs from:", h$ccsString(h$currentThread.ccs))
    TRACE("             to:", h$ccsString(ccs))
    h$currentThread.ccs = ccs;
    h$reportCurrentCcs();
}

function h$enterThunkCCS(ccsthunk) {
  ASSERT(ccsthunk !== null && ccsthunk !== undefined, "ccsthunk is null or undefined")
  TRACE("entering ccsthunk:", h$ccsString(ccsthunk))
  h$currentThread.ccs = ccsthunk;
  h$reportCurrentCcs();
}

function h$enterFunCCS(ccsapp, // stack at call site
                       ccsfn   // stack of function
                       ) {
  ASSERT(ccsapp !== null && ccsapp !== undefined, "ccsapp is null or undefined")
  ASSERT(ccsfn  !== null && ccsfn  !== undefined, "ccsfn is null or undefined")
  TRACE("ccsapp:", h$ccsString(ccsapp))
  TRACE("ccsfn:", h$ccsString(ccsfn))

  // common case 1: both stacks are the same
  if (ccsapp === ccsfn) {
    return;
  }

  // common case 2: the function stack is empty, or just CAF
  if (ccsfn.prevStack === h$CCS_MAIN) {
    return;
  }

  // FIXME: do we need this?
  h$currentThread.ccs = h$CCS_OVERHEAD;

  // common case 3: the stacks are completely different (e.g. one is a
  // descendent of MAIN and the other of a CAF): we append the whole
  // of the function stack to the current CCS.
  if (ccsfn.root !== ccsapp.root) {
    h$currentThread.ccs = h$appendCCS(ccsapp, ccsfn);
    h$reportCurrentCcs();
    return;
  }

  // uncommon case 4: ccsapp is deeper than ccsfn
  if (ccsapp.depth > ccsfn.depth) {
    var tmp = ccsapp;
    var dif = ccsapp.depth - ccsfn.depth;
    for (var i = 0; i < dif; i++) {
      tmp = tmp.prevStack;
    }
    h$currentThread.ccs = h$enterFunEqualStacks(ccsapp, tmp, ccsfn);
    h$reportCurrentCcs();
    return;
  }

  // uncommon case 5: ccsfn is deeper than CCCS
  if (ccsfn.depth > ccsapp.depth) {
    h$currentThread.ccs = h$enterFunCurShorter(ccsapp, ccsfn, ccsfn.depth - ccsapp.depth);
    h$reportCurrentCcs();
    return;
  }

  // uncommon case 6: stacks are equal depth, but different
  h$currentThread.ccs = h$enterFunEqualStacks(ccsapp, ccsapp, ccsfn);
  h$reportCurrentCcs();
}

function h$appendCCS(ccs1, ccs2) {
  if (ccs1 === ccs2) {
    return ccs1;
  }

  if (ccs2 === h$CCS_MAIN || ccs2.cc.isCaf) {
    // stop at a CAF element
    return ccs1;
  }

  return h$pushCostCentre(h$appendCCS(ccs1, ccs2.prevStack), ccs2.cc);
}

function h$enterFunCurShorter(ccsapp, ccsfn, n) {
  if (n === 0) {
    ASSERT(ccsapp.length === ccsfn.length, "ccsapp.length !== ccsfn.length")
    return h$enterFunEqualStacks(ccsapp, ccsapp, ccsfn);
  } else {
    ASSERT(ccsfn.depth > ccsapp.depth, "ccsfn.depth <= ccsapp.depth")
    return h$pushCostCentre(h$enterFunCurShorter(ccsapp, ccsfn.prevStack, n-1), ccsfn.cc);
  }
}

function h$enterFunEqualStacks(ccs0, ccsapp, ccsfn) {
  ASSERT(ccsapp.depth === ccsfn.depth, "ccsapp.depth !== ccsfn.depth")
  if (ccsapp === ccsfn) return ccs0;
  return h$pushCostCentre(h$enterFunEqualStacks(ccs0, ccsapp.prevStack, ccsfn.prevStack), ccsfn.cc);
}

function h$pushCostCentre(ccs, cc) {
  TRACE("pushing cost centre", cc.label, "to", h$ccsString(ccs))
  if (ccs === null) {
    // when is ccs null?
    return new h$CCS(ccs, cc);
  }

  if (ccs.cc === cc) {
    return ccs;
  } else {
    var temp_ccs = h$checkLoop(ccs, cc);
    if (temp_ccs !== null) {
      return temp_ccs;
    }
    return new h$CCS(ccs, cc);
  }
}

function h$checkLoop(ccs, cc) {
  while (ccs !== null) {
    if (ccs.cc === cc)
      return ccs;
    ccs = ccs.prevStack;
  }
  return null;
}

//
// Emulating pointers for cost-centres and cost-centre stacks
//

var h$ccsCC_offset       = 4;  // ccs->cc
var h$ccsPrevStackOffset = 8;  // ccs->prevStack

var h$ccLabel_offset     = 4;  // cc->label
var h$ccModule_offset    = 8;  // cc->module
var h$ccsrcloc_offset    = 12; // cc->srcloc

function h$buildCCPtr(o) {
  // last used offset is 12, so we need to allocate 20 bytes
  ASSERT(o !== null)
  var cc = h$newByteArray(20);
#ifdef PROFILING
  cc.myTag = "cc pointer";
#endif
  PUT_ADDR(cc, h$ccLabel_offset,  h$encodeUtf8(o.label),  0);
  PUT_ADDR(cc, h$ccModule_offset, h$encodeUtf8(o.module), 0);
  PUT_ADDR(cc, h$ccsrcloc_offset, h$encodeUtf8(o.srcloc), 0);
  return cc;
}

function h$buildCCSPtr(o) {
  ASSERT(o !== null)
  // last used offset is 8, allocate 16 bytes
  var ccs = h$newByteArray(16);
#ifdef PROFILING
  ccs.myTag = "ccs pointer";
#endif
  ccs.arr = [];
  if (o.prevStack !== null) {
    ccs.arr[h$ccsPrevStackOffset] = [h$buildCCSPtr(o.prevStack), 0];
  }
  // FIXME: we may need this part:
  // else {
  //   ccs.arr[h$ccsPrevStackOffset] = [null, 0];
  // }
  ccs.arr[h$ccsCC_offset] = [h$buildCCPtr(o.cc), 0];
  return ccs;
}

// run the action with an empty CCS
function h$clearCCS(a) {
  throw new Error("ClearCCSOp not implemented");
}
