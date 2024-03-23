//#OPTIONS: CPP

/*
  Do garbage collection where the JavaScript GC doesn't suffice or needs some help:

  - run finalizers for weak references
  - find unreferenced CAFs and reset them (unless h$retainCAFs is set)
  - shorten stacks that are mostly empty
  - reset unused parts of stacks to null
  - reset registers to null
  - reset return variables to null
  - throw exceptions to threads that are blocked on an unreachable MVar/STM transaction
  - drop unnecessary references for selector thunks

  The gc uses the .m field to store its mark in all the objects it marks. for heap objects,
  the .m field is also used for other things, like stable names, the gc only changes
  the two least significant bits for these.

  The gc starts with all threads as roots in addition to callbacks passed to JavaScript
  that that are retained. If you have custom JavaScript data structures that contain
  Haskell heap object references, you can use extensible retention to find these
  references and add thm to the work queue. h$registerExtensibleRetensionRoot(f) calls
  f(currentMark) at the start of every gc, h$registerExtensibleRetention(f) calls f(o, currentMark)
  for every unknown object found on the Haskell heap.

  Extensible retention is a low-level mechanism and should typically only be used by
  bindings that guarantee that the shape of the JS objects exactly matches what
  the scanner expects. Care should be taken to make sure that the objects never
  escape the reach of the scanner.

  Having correct reachability information is important, even if you choose to turn off
  features like weak references and deallocating CAFs in production, since it helps
  debugging by providing the profiler with accurate data and by properly raising
  exceptions when threads become blocked indefinitely, usually indicating a bug or
  memory leak.

  assumptions:
  - all threads suspended, no active registers
  - h$currentThread == null or at least unused:
       1. all reachable threads must be in h$threads or h$blocked
       2. no registers contain any usable value
  notes:
  - gc() may replace the stack of any thread, make sure to reload h$stack after gc()
*/

/*
  fixme, todo:
  - mark posted exceptions to thread
*/

#ifdef GHCJS_TRACE_GC
function h$traceGC() { h$log.apply(h$log, arguments); }
#define TRACE_GC(args...) h$traceGC(args)
#else
#define TRACE_GC(args...)
#endif

// these macros use a local mark variable
#define IS_MARKED(obj) ((typeof obj.m === 'number' && (obj.m & 3) === mark) || (typeof obj.m === 'object' && ((obj.m.m & 3) === mark)))
#define IS_MARKED_M(obj) ((obj.m & 3) === mark)
#define MARK_OBJ(obj) if(typeof obj.m === 'number') obj.m = (obj.m&-4)|mark; else obj.m.m = (obj.m.m & -4)|mark;

var h$gcMark = 2; // 2 or 3 (objects initialized with 0)

#ifdef GHCJS_TRACE_GC
var h$gcTime = 0;
#endif

#ifdef GHCJS_RETAIN_CAFS
var h$retainCAFs = true;
#else
var h$retainCAFs = false;
#endif

// FIXME remove this? declared in rts.js now
// var h$CAFs = [];
// var h$CAFsReset = [];

//
var h$extensibleRetentionRoots     = [];
var h$extensibleRetentionCallbacks = [];


/*
   after registering an extensible extension root f,
   f(currentMark) is called at the start of each gc invocation and is
   expected to return an array with Haskell heap objects
   to be treated as extra roots.
 */
function h$registerExtensibleRetentionRoot(f) {
    h$extensibleRetentionRoots.push(f);
}

function h$unregisterExtensibleRetentionRoot(f) {
    h$extensibleRetentionRoots = h$extensibleRetentionRoots.filter(function(g) { return f !== g; });
}

/*
  after registering an extensible retention callback f,
  f(o, currentMark) is called for every unknown object encountered on the
  Haskell heap. f should return an array with found objects. If no objects
  are found, f should return a boolean indicating whether the gc should skip
  processing the objects with other extensible retention callbacks.

  The gc may encounter the same object multiple times during the same scan,
  so a callback should attempt to quickly return if the object has been scanned
  already.

   return value:
     - array          scan objects contained in array, do not call other extension callbacks
     - true           do not call other extension callbacks with this object
     - false          call other extension callbacks with this object

  Use -DGHCJS_TRACE_GC_UNKNOWN to find the JavaScript objects reachable
  (through JSVal) on the Haskell heap for which none of the registered
  extensible retention callbacks has returned true or an array.
 */
function h$registerExtensibleRetention(f) {
    h$extensibleRetentionCallbacks.push(f);
}

function h$unregisterExtensibleRetention(f) {
    h$extensibleRetentionCallbacks = h$extensibleRetentionCallbacks.filter(function(g) { return f !== g; });
}

// check whether the object is marked by the latest gc
function h$isMarked(obj) {
  return (typeof obj === 'object' || typeof obj === 'function') &&
        ((typeof obj.m === 'number' && (obj.m & 3) ===  h$gcMark) || (obj.m && typeof obj.m === 'object' && obj.m.m === h$gcMark));
}

// do a quick gc of a thread:
// - reset the stack (possibly shrinking storage for it)
// - reset all global data
// checks all known threads if t is null, but not h$currentThread
function h$gcQuick(t) {
#ifdef GHCJS_DISABLE_GC
    return;
#endif
    if(h$currentThread !== null) throw "h$gcQuick: GC can only run when no thread is running";
#ifdef GHCJS_TRACE_GC
    var start = Date.now();
#endif
    h$resetRegisters();
    h$resetResultVars();
    var i;
    if(t !== null) { // reset specified threads
        if(t instanceof h$Thread) {  // only thread t
            h$resetThread(t);
        } else { // assume it's an array
            for(var i=0;i<t.length;i++) h$resetThread(t[i]);
        }
    } else { // all threads, h$currentThread assumed unused
        var nt, runnable = h$threads.iter();
        while((nt = runnable()) !== null) h$resetThread(nt);
        var iter = h$blocked.iter();
        while((nt = iter.next()) !== null) h$resetThread(nt);
    }
#ifdef GHCJS_TRACE_GC
    var time = Date.now() - start;
    h$gcTime += time;
    TRACE_GC("time (quick): " + time + "ms")
    TRACE_GC("time (total): " + h$gcTime + "ms")
#endif
}

// run full marking for threads in h$blocked and h$threads, optionally t if t /= null
#ifdef GHCJS_TRACE_GC
var h$marked = 0;
#endif
function h$gc(t) {
#ifdef GHCJS_DISABLE_GC
    return;
#endif
#ifndef GHCJS_BROWSER
    // fixme, should enable again later when proper CAF management
    // and retention of the standard handles in GHCJSi work
    if(h$isGHCJSi()) return;
#endif

    if(h$currentThread !== null) throw "h$gc: GC can only be run when no thread is running";
#ifdef GHCJS_TRACE_GC
    h$marked = 0;
    TRACE_GC("gc: " + (t!==null?h$threadString(t):"null"))
    var start = Date.now();
#endif
    TRACE_GC("full gc of thread " + h$threadString(t))
    h$resetRegisters();
    h$resetResultVars();
    h$gcMark = 5-h$gcMark;
    var i;
    TRACE_GC("scanning extensible retention roots")
    for(i=h$extensibleRetentionRoots.length-1;i>=0;i--) {
      var a = h$extensibleRetentionRoots[i](h$gcMark);
      if(a) h$follow(a, a.length-1);
    }
    TRACE_GC("scanning threads, runnable: " + h$threads.length() + " blocked: " + h$blocked.size() + " t: " + t)

    // mark al runnable threads and the running thread
    if(t !== null) {
	h$markThread(t);
	h$resetThread(t);
    }
    var nt, runnable = h$threads.iter();
    while((nt = runnable()) !== null) {
	h$markThread(nt);
	h$resetThread(nt);
    }

    // some blocked threads are always considered reachable, mark them
    //   - delayed threads
    //   - threads blocked on async FFI
    var iter = h$blocked.iter();
    while((nt = iter.next()) !== null) {
        if(nt.delayed ||
	   (nt.blockedOn instanceof h$MVar && nt.stack && nt.stack[nt.sp] === h$unboxFFIResult)) {
            h$markThread(nt);
        }
	h$resetThread(nt);
    }
    TRACE_GC("scanning permanent retention roots")
    iter = h$extraRoots.iter();
    while((nt = iter.next()) !== null) h$follow(nt.root);

    TRACE_GC("scanning stable pointers")
    for(i=0;i<h$stablePtrData.length;i++) {
      if(h$stablePtrData[i]) h$follow(h$stablePtrData[i]);
    }

    // clean up threads waiting on unreachable synchronization primitives
    h$resolveDeadlocks();

    // clean up unreachable weak refs
    var toFinalize = h$markRetained();
    h$finalizeWeaks(toFinalize);

    h$finalizeCAFs();   // restore all unreachable CAFs to unevaluated state

    var now = Date.now();
    h$lastGc = now;
#ifdef GHCJS_TRACE_GC
    var time = now - start;
    h$gcTime += time;
    TRACE_GC("time: " + time + "ms")
    TRACE_GC("time (total): " + h$gcTime + "ms")
    TRACE_GC("marked objects: " + h$marked)
#endif
    h$debugAlloc_verifyReachability(h$gcMark);
}

function h$markWeaks() {
  var i, w, marked, mark = h$gcMark;
  do {
    marked = false;
    for (i = 0; i < h$weakPointerList.length; ++i) {
      w = h$weakPointerList[i];
      if (IS_MARKED_M(w.keym)) {
	if (w.val !== null && !IS_MARKED(w.val)) {
          h$follow(w.val);
	  marked = true;
	}
	if (w.finalizer !== null && !IS_MARKED(w.finalizer)) {
          h$follow(w.finalizer);
	  marked = true;
	}
      }
    }
  } while(marked);
}


function h$markRetained() {
    var iter, marked, w, i, mark = h$gcMark;
    var newList = [];
    var toFinalize = [];

    /*
      2. Scan the Weak Pointer List. If a weak pointer object has a key that is
      marked (i.e. reachable), then mark all heap reachable from its value
      or its finalizer, and move the weak pointer object to a new list
    */
    do {
        TRACE_GC("mark retained iteration 1/2")
        marked = false;

        for (i = 0; i < h$weakPointerList.length; ++i) {
            w = h$weakPointerList[i];
            if (w === null) {
                // don't handle items deleted in earlier iteration
                continue;
            }
            if (IS_MARKED_M(w.keym)) {
                if (w.val !== null && !IS_MARKED(w.val)) {
                    h$follow(w.val);
                }

                if (w.finalizer !== null && !IS_MARKED(w.finalizer)) {
                    h$follow(w.finalizer);
                }

                newList.push(w);
                // instead of removing the item from the h$weakpointerList
                // we set it to null if we push it to newList.
                h$weakPointerList[i] = null;

                marked = true;
            }
        }

        /*
           3. Repeat from step (2), until a complete scan of Weak Pointer List finds
              no weak pointer object with a marked keym.
        */
    } while(marked);


    /*
      4. Scan the Weak Pointer List again. If the weak pointer object is reachable
         then tombstone it. If the weak pointer object has a finalizer then move
         it to the Finalization Pending List, and mark all the heap reachable
         from the finalizer. If the finalizer refers to the key (and/or value),
         this step will "resurrect" it.
    */

    for (i = 0; i < h$weakPointerList.length; ++i) {
        w = h$weakPointerList[i];
        if (w === null) {
            // don't handle items deleted in step 2
            continue;
        }

        TRACE_GC("mark retained iteration 2/2")
        if(w.val !== null) {
            w.val = null;
        }

        if(w.finalizer !== null) {
            if(!IS_MARKED(w.finalizer)) {
                TRACE_GC("following finalizer")
                h$follow(w.finalizer);
            }
            toFinalize.push(w);
        }
    }

    /*
       5. The list accumulated in step (3) becomes the new Weak Pointer List.
          Mark any unreachable weak pointer objects on this list as reachable.
    */
    h$weakPointerList = newList;

    // marking the weak pointer objects as reachable is not necessary

    return toFinalize;
}

function h$markThread(t) {
    var mark = h$gcMark;
    TRACE_GC("marking thread: " + h$threadString(t))
    if(IS_MARKED(t)) return;
    h$follow(t);
}

#define ADDW(x) work[w++] = x;
#define ADDW2(x,y) { work[w++] = x; work[w++] = y; }
#define ADDW3(x,y,z) { work[w++] = x; work[w++] = y; work[w++] = z; }
#define ADDW4(x,y,z,v) { work[w++] = x; work[w++] = y; work[w++] = z; work[w++] = v; }

// big object, not handled by 0..7 cases
// keep out of h$follow to prevent deopt
function h$followObjGen(c, work, w) {
   ADDW(c.d1);
   var d = c.d2;
   for(var x in d) {
//              if(d.hasOwnProperty(x)) {
     ADDW(d[x]);
//              }
   }
    return w;
}

// follow all references in the object obj and mark them with the current mark
// if sp is a number, obj is assumed to be an array for which indices [0..sp] need
// to be followed (used for thread stacks)
function h$follow(obj, sp) {
    var i, ii, iter, c, work, w;
#ifdef GHCJS_TRACE_GC
    var start = Date.now();
#endif
    TRACE_GC("following")
    var work, mark = h$gcMark;
    if(typeof sp === 'number') {
        work = obj.slice(0, sp+1);
        w = sp + 1;
    } else {
        work = [obj];
        w = 1;
    }
    while(w > 0) {
        TRACE_GC("work length: " + work.length + " w: " + w)
        c = work[--w];
        TRACE_GC("[" + work.length + "] mark step: " + typeof c)
#ifdef GHCJS_TRACE_GC
        if(typeof c === 'object') {
            if(c !== null) {
                TRACE_GC("object: " + c.toString())
                TRACE_GC("object props: " + h$collectProps(c))
                TRACE_GC("object mark: " + c.m + " (" + typeof(c.m) + ") (current: " + mark + ")")
            } else {
                TRACE_GC("object: " + c)
            }
        }
#endif
        if(c !== null && c !== undefined && typeof c === 'object' && ((typeof c.m === 'number' && (c.m&3) !== mark) || (typeof c.m === 'object' && c.m !== null && typeof c.m.m === 'number' && (c.m.m&3) !== mark))) {
            var doMark = false;
            var cf = c.f;
            TRACE_GC("first accepted")
            if(typeof cf === 'function' && (typeof c.m === 'number' || typeof c.m === 'object')) {
                TRACE_GC("marking heap object: " + c.f.n + " size: " + c.f.size)
                // only change the two least significant bits for heap objects
                MARK_OBJ(c);
                // dynamic references
                var d = c.d2;
                switch(cf.size) {
                case 0: break;
                case 1: ADDW(c.d1); break;
                case 2: ADDW2(c.d1, d); break;
                case 3: var d3=c.d2; ADDW3(c.d1, d3.d1, d3.d2); break;
                case 4: var d4=c.d2; ADDW4(c.d1, d4.d1, d4.d2, d4.d3); break;
                case 5: var d5=c.d2; ADDW4(c.d1, d5.d1, d5.d2, d5.d3); ADDW(d5.d4); break;
                case 6: var d6=c.d2; ADDW4(c.d1, d6.d1, d6.d2, d6.d3); ADDW2(d6.d4, d6.d5); break;
                case 7: var d7=c.d2; ADDW4(c.d1, d7.d1, d7.d2, d7.d3); ADDW3(d7.d4, d7.d5, d7.d6); break;
                case 8: var d8=c.d2; ADDW4(c.d1, d8.d1, d8.d2, d8.d3); ADDW4(d8.d4, d8.d5, d8.d6, d8.d7); break;
                case 9: var d9=c.d2; ADDW4(c.d1, d9.d1, d9.d2, d9.d3); ADDW4(d9.d4, d9.d5, d9.d6, d9.d7); ADDW(d9.d8); break;
                case 10: var d10=c.d2; ADDW4(c.d1, d10.d1, d10.d2, d10.d3); ADDW4(d10.d4, d10.d5, d10.d6, d10.d7); ADDW2(d10.d8, d10.d9); break;
                case 11: var d11=c.d2; ADDW4(c.d1, d11.d1, d11.d2, d11.d3); ADDW4(d11.d4, d11.d5, d11.d6, d11.d7); ADDW3(d11.d8, d11.d9, d11.d10); break;
                case 12: var d12=c.d2; ADDW4(c.d1, d12.d1, d12.d2, d12.d3); ADDW4(d12.d4, d12.d5, d12.d6, d12.d7); ADDW4(d12.d8, d12.d9, d12.d10, d12.d11); break;
                default: w = h$followObjGen(c,work,w);
                }
                // static references
                var s = cf.s;
                if(s !== null) {
                    TRACE_GC("adding static marks")
                    for(var i=0;i<s.length;i++) ADDW(s[i]);
                }
            } else if(typeof c.len === 'number' && c.buf instanceof ArrayBuffer) {
                TRACE_GC("marking ByteArray")
                MARK_OBJ(c);
            } else if(c instanceof h$Weak) {
                MARK_OBJ(c);
            } else if(c instanceof h$MVar) {
                TRACE_GC("marking MVar")
                MARK_OBJ(c);
                iter = c.writers.iter();
                while((ii = iter()) !== null) {
		    ADDW(ii[1]); // value
		    ADDW(ii[0]); // thread
		}
		iter = c.readers.iter();
		while((ii = iter()) !== null) {
		    ADDW(ii);
		}
	        if(c.waiters) {
		  for(i=c.waiters.length-1;i>=0;i--) {
		    ADDW(c.waiters[i]);
		  }
		}
                if(c.val !== null && !IS_MARKED(c.val)) ADDW(c.val);
            } else if(c instanceof h$MutVar) {
                TRACE_GC("marking MutVar")
                MARK_OBJ(c);
                ADDW(c.val);
            } else if(c instanceof h$TVar) {
                TRACE_GC("marking TVar")
                MARK_OBJ(c);
                ADDW(c.val);
		iter = c.blocked.iter();
		while((ii = iter.next()) !== null) {
		    ADDW(ii);
		}
		if(c.invariants) {
		    iter = c.invariants.iter();
		    while((ii = iter.next()) !== null) {
			ADDW(ii);
		    }
		}
            } else if(c instanceof h$Thread) {
                TRACE_GC("marking Thread")
                MARK_OBJ(c);
                if(c.stack) {
                    for(i=c.sp;i>=0;i--) {
			ADDW(c.stack[i]);
		    }
                }
		for(i=0;i<c.excep.length;i++) {
		    ADDW(c.excep[i][0]); // the posting thread
		    ADDW(c.excep[i][1]); // the exception
		}
            } else if(c instanceof h$Transaction) {
                // - the accessed TVar values don't need to be marked
                // - parents are also on the stack, so they should've been marked already
                TRACE_GC("marking STM transaction")
                MARK_OBJ(c);
                for(i=c.invariants.length-1;i>=0;i--) {
		    ADDW(c.invariants[i].action);
		}
                ADDW(c.action);
                iter = c.tvars.iter();
                while((ii = iter.nextVal()) !== null) {
		    ADDW(ii.val);
		}
            } else if(c instanceof Array && c.__ghcjsArray) {
		// only for Haskell arrays with lifted values
                MARK_OBJ(c);
                TRACE_GC("marking array")
                for(i=0;i<c.length;i++) {
                    var x = c[i];
                    if(typeof x === 'object' && x !== null && !IS_MARKED(x)) {
		        ADDW(x);
		    }
                }
            } else if(typeof c === 'object') {
                TRACE_GC("extensible retention marking")
#ifdef GHCJS_TRACE_GC_UNKNOWN
                var extensibleMatched = false;
#endif
                for(i=h$extensibleRetentionCallbacks.length-1;i>=0;i--) {
                    var x = h$extensibleRetentionCallbacks[i](c, mark);
                    if(x === false) continue;
#ifdef GHCJS_TRACE_GC_UNKNOWN
                    extensibleMatched = true;
#endif
                    if(x !== true) {
                      var j;
                      for(j=x.length-1;j>=0;j--) {
		          ADDW(x[j]);
		      }
                    }
                    break;
                }
#ifdef GHCJS_TRACE_GC_UNKNOWN
                if(!extensibleMatched) {
                    TRACE_GC("unknown object: " + h$collectProps(c))
                }
#endif
            } // otherwise: not an object, no followable values
        }
    }
    TRACE_GC("h$follow: " + (Date.now()-start) + "ms")
}

// resetThread clears the stack above the stack pointer
// and shortens the stack array if there is too much
// unused space
function h$resetThread(t) {
#ifdef GHCJS_TRACE_GC
    var start = Date.now();
#endif
    var stack = t.stack;
    if(!stack) return;
    var sp = t.sp;
    if(stack.length - sp > sp && stack.length > 100) {
        t.stack = t.stack.slice(0,sp+1);
    } else {
        for(var i=sp+1;i<stack.length;i++) {
            stack[i] = null;
        }
    }
    TRACE_GC("h$resetThread: " + (Date.now()-start) + "ms")
}

/*
   Post exceptions to all threads that are waiting on an unreachable synchronization
   object and haven't been marked reachable themselves.

   All woken up threads are marked.
 */
function h$resolveDeadlocks() {
    TRACE_GC("resolving deadlocks")
    var kill, t, iter, bo, mark = h$gcMark;
    do {
        h$markWeaks();
        // deal with unreachable blocked threads: kill an unreachable thread and restart the process
        kill = null;
        iter = h$blocked.iter();
        while((t = iter.next()) !== null) {
            // we're done if the thread is already reachable
            if(IS_MARKED(t)) continue;

            // check what we're blocked on
            bo = t.blockedOn;
            if(bo instanceof h$MVar) {
                // blocked on MVar
                if(bo.m === mark) throw "assertion failed: thread should have been marked";
                // MVar unreachable
                kill = h$ghczminternalZCGHCziInternalziJSziPrimziInternalziblockedIndefinitelyOnMVar;
                break;
            } else if(t.blockedOn instanceof h$TVarsWaiting) {
                // blocked in STM transaction
                kill = h$ghczminternalZCGHCziInternalziJSziPrimziInternalziblockedIndefinitelyOnSTM;
                break;
            } else {
                // blocked on something else, we can't do anything
            }
        }
        if(kill) {
            h$killThread(t, kill);
            h$markThread(t);
        }
    } while(kill);
}

// register a CAF (after initialising the heap object)
function h$addCAF(o) {
  h$CAFs.push(o);
  h$CAFsReset.push([o.f, o.d1, o.d2]);
}

// reset unreferenced CAFs to their initial value
function h$finalizeCAFs() {
    if(h$retainCAFs) return;
#ifdef GHCJS_TRACE_GC
    var start = Date.now();
#endif
    var mark = h$gcMark;
    for(var i=0;i<h$CAFs.length;i++) {
        var c = h$CAFs[i];
        if(c.m & 3 !== mark) {
            var cr = h$CAFsReset[i];
            if(c.f !== cr[0]) { // has been updated, reset it
                TRACE_GC("resetting CAF: " + cr.n)
                c.f = cr[0];
                c.d1 = cr[1];
                c.d2 = cr[2];
            }
        }
    }
    TRACE_GC("h$finalizeCAFs: " + (Date.now()-start) + "ms")
}

