/*
  debugging tools that depend on node.js extensions

  compile with -DGHCJS_DEBUG_ALLOC to use

  compile additionally with -DGHCJS_DEBUG_ALLOC_ALWAYS_ENABLE to enable
  allocation debugging even if the JavaScript runtime does not support
  weak references with observable deadness. This causes allocation
  debugging to run with reduced functionality (h$debugAlloc_shouldBeDead
  is not available) and keeps strong references to everything allocaded
  between h$gc calls.
 */

// public API

// called after (GHCJS) GC, all objects reachable from the Haskell
// runtime must have been marked with the given mark
function h$debugAlloc_verifyReachability(mark) {
#ifdef GHCJS_DEBUG_ALLOC
  h$debugAlloc_init_internal();
  h$debugAlloc_verifyReachability_internal(mark);
#endif
}

// called after creating a new Heap object or RTS primitive
function h$debugAlloc_notifyAlloc(obj) {
#ifdef GHCJS_DEBUG_ALLOC
  h$debugAlloc_init_internal();
  h$debugAlloc_notifyAlloc_internal(obj);
#endif
}

// called when an object is used
function h$debugAlloc_notifyUse(obj) {
#ifdef GHCJS_DEBUG_ALLOC
  h$debugAlloc_init_internal();
  h$debugAlloc_notifyUse_internal(obj);
#endif
}

// private API

#ifdef GHCJS_DEBUG_ALLOC

var h$debugAlloc;

function h$debugAlloc_init_internal() {
  if(!h$debugAlloc) {
    h$debugAlloc =
      { enabled: false
      // Set of weak references to everything that's been allocated
      , allocatedHeapObjects: null
      // reference to the node.js weak module
      , makeWeak: null
      // exception while trying to load the node.js weak module
      , makeWeakError: null
      // each registered allocated heap object gets a unique sequence number
      , allocCount: 0
      // these objects have been seen with the wrong mark at least once (Set of weak refs)
      // each object has a __ghcjsShouldBeDeadSince timestamp indicating the first time
      // the object was unreachable from the Haskell heap
      , shouldBeDead: null
      // these objects have been seen with the wrong mark at least once, and have been
      // used after that (Map of sequence no -> heap object)
      //
      // this set should remain empty.
      , shouldBeDead_reported: null
      // these sequence ids of items that have been in the shouldBeDead_reported set
      // this should also be empty
      , shouldBeDead_reported_ids: null
    };

#ifndef GHCJS_BROWSER
    if(h$isNode) {
      try {
        // the 'weak' package provides the same functionality, but segfaults
        // try this first
        h$debugAlloc.makeWeak = require('weak-napi');
        h$debugAlloc.enabled = true;
      } catch(e) {
        h$debugAlloc.makeWeakError = e;
      }
      /*
      if(!h$debugAlloc.makeWeak) {
      try {
        // fall back to 'weak'
        h$debugAlloc.makeWeak = require('weak');
        h$debugAlloc.enabled = true;
      } catch(e) {
        h$debugAlloc.makeWeakError = e;
      }
    }*/
    }
#endif
#ifdef GHCJS_DEBUG_ALLOC_ALWAYS_ENABLE
    h$debugAlloc.enabled = true;
#endif
    if(h$debugAlloc.enabled) {
      h$debugAlloc.allocatedHeapObjects = new Set();
      if(h$debugAlloc.makeWeak) {
        h$debugAlloc.shouldBeDead = new Set();
      }
      h$debugAlloc.shouldBeDead_reported = new Map();
      h$debugAlloc.shouldBeDead_reported_ids = new Set();
    }
  }
}

function h$debugAlloc_notifyAlloc_internal(obj) {
  if(!h$debugAlloc.enabled) return;
  // test if already notified
  if(typeof obj.__ghcjsDebugAllocSequenceNo == 'number') return;
  obj.__ghcjsDebugAllocSequenceNo = h$debugAlloc.allocCount++;
  obj.__ghcjsDebugAllocStack = new Error();

  // wrap all own properties in a getter and setter
  // use these to notify the allocation debugger of each use
  for(var p in obj) {
    if(!p.startsWith('__') && obj.hasOwnProperty(p)) {
      (function(pobj, pp) {
        var pw = '__alloc_wrapped_' + pp;
        pobj[pw] = pobj[pp];
        Object.defineProperty(pobj, pp,
          { get: function() {
                   h$debugAlloc_notifyUse_internal(this);
                   return this[pw];
                 }
          , set: function(v) {
                  h$debugAlloc_notifyUse_internal(this);
                  this[pw] = v;
                 }
          });
        })(obj, p);
    }
  }

  // insert into our set of allocated heap objects, use a weak ref if supported
  if(h$debugAlloc.makeWeak) {
    h$debugAlloc.allocatedHeapObjects.add(h$debugAlloc.makeWeak(obj));
  } else {
    h$debugAlloc.allocatedHeapObjects.add(obj);
  }
}

function h$debugAlloc_filterReport(obj) {
  var stk = obj.__ghcjsDebugAllocStack ? obj.__ghcjsDebugAllocStack.stack : '';
  if(!(stk.indexOf('h$runThreadSlice') >= 0)) return true;

  // if(obj.hasOwnProperty('f') && obj.hasOwnProperty('d1') && obj.hasOwnProperty('d2')) {
  // }
  // if(stk.indexOf(' at h$d ') >= 0) return true;
  // if(stk.indexOf(' at h$makeEnum ') >= 0) return true;
  return false;
}

var h$debugAlloc_notify_recursive = false;
function h$debugAlloc_notifyUse_internal(obj) {
  if(!h$debugAlloc.enabled) return;
  if(h$debugAlloc_notify_recursive) return;
  try {
    h$debugAlloc_notify_recursive = true;
    if(typeof obj === 'object' && obj && typeof obj.__ghcjsShouldBeDeadSince === 'number' && !h$debugAlloc_filterReport(obj)) {
      var seqNo = obj.__ghcjsDebugAllocSequenceNo;
      if(!h$debugAlloc.shouldBeDead_reported_ids.has(seqNo)) {
        if(h$debugAlloc.makeWeak) {
          h$debugAlloc.shouldBeDead_reported.set(seqNo, obj); // h$debugAlloc.makeWeak(obj));
        }
        h$debugAlloc.shouldBeDead_reported_ids.add(seqNo);
        // should we do more reporting, output the CCS or JS stack of the allocation point perhaps?
      }
    }
  } finally {
    h$debugAlloc_notify_recursive = false;
  }
}

// call this immediately after h$gc
function h$debugAlloc_verifyReachability_internal(mark) {
  if(!h$debugAlloc.enabled) return;
  if(typeof global == 'object' && global && global.gc) {
    global.gc();
  }
  var timestamp = Date.now();
  var weak, elem, key;
  function getMark(obj) {
    if(typeof obj.m === 'number') {
      return obj.m;
    } else if(typeof elem.m === 'object' && elem.m) {
      return obj.m.m;
    } else {
      return -1;
    }
  }
  if(h$debugAlloc.makeWeak) {
    // keep track of things with assistance from weaks in the JS runtime, yay!
    for(weak of h$debugAlloc.allocatedHeapObjects) {
      elem = h$debugAlloc.makeWeak.get(weak);
      if(elem === undefined) {
        h$debugAlloc.allocatedHeapObjects.delete(weak);
      } else {
        if(getMark(elem) !== mark) {
          elem.__ghcjsShouldBeDeadSince = timestamp;
          h$debugAlloc.shouldBeDead.add(h$debugAlloc.makeWeak(elem));
        }
      }
    }
    // clean up dead refs
    for(weak of h$debugAlloc.shouldBeDead) {
      if(h$debugAlloc.makeWeak.isDead(weak)) {
        h$debugAlloc.shouldBeDead.delete(weak);
      }
    }
    /* we store the original objects now
    for([key, weak] of h$debugAlloc.shouldBeDead_reported) {
      if(h$debugAlloc.makeWeak.isDead(weak)) {
        h$debugAlloc.shouldBeDead_reported.delete(key);
      }
    } */
  } else {
    // no support for weaks in the JS runtime
    // we don't keep track of shouldBeDead, since that would leak everything ever allocated
    // but we can still record use-after-unreachable cases
    for(elem of h$debugAlloc.allocatedHeapObjects) {
      if(getMark(elem) !== mark) {
        h$debugAlloc.allocatedHeapObjects.delete(elem);
        elem.__ghcjsShouldBeDeadSince = timestamp;
      }
    }
  }
}
#endif

/*
var h$debug = {};

function h$loadDebugTools() {
  function load(m, p) {
    if(h$isNode) {
      try {
        var t = require(m);
        h$debug[p] = t;
      } catch(e) { }
    }
  }
  load('gc-stats', 'gcStats');
  load('v8-natives', 'v8');
  var t;
  if(h$isNode) {
    try {
      t = require('gc-stats');
      h$debug.gcStats = t;
    } catch(e) { }
    try {
      require('');
    } catch(e) { }
  }
}
*/
