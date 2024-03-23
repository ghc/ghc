//#OPTIONS: CPP

// preemptive threading support

// run gc when this much time has passed (ms)
#ifndef GHCJS_GC_INTERVAL
#define GHCJS_GC_INTERVAL 1000
#endif

// preempt threads after the scheduling quantum (ms)
#ifndef GHCJS_SCHED_QUANTUM
#define GHCJS_SCHED_QUANTUM 25
#endif

// check sched quantum after 10*GHCJS_SCHED_CHECK calls
#ifndef GHCJS_SCHED_CHECK
#define GHCJS_SCHED_CHECK 1000
#endif

// yield to js after running haskell for GHCJS_BUSY_YIELD ms
#ifndef GHCJS_BUSY_YIELD
#define GHCJS_BUSY_YIELD 500
#endif

// Watch for insertion of null or undefined in the stack
//#define GHCJS_DEBUG_STACK

#ifdef GHCJS_TRACE_SCHEDULER
function h$logSched() { if(arguments.length == 1) {
                          if(h$currentThread != null) {
                            h$log((Date.now()/1000) + " sched: " + h$threadString(h$currentThread) +
                                "[" + h$currentThread.mask + "," +
                                (h$currentThread.interruptible?1:0) + "," +
                                h$currentThread.excep.length +
                                "] -> " + arguments[0]);
                          } else {
                            h$log("sched: " + h$threadString(h$currentThread) + " -> " + arguments[0]);
                          }
                        } else {
                          h$log.apply(log,arguments);
                        }
                      }
#define TRACE_SCHEDULER(args...) h$logSched(args)
#else
#define TRACE_SCHEDULER(args...)
#endif

#ifdef GHCJS_TRACE_CALLS
// print function to be called from trampoline and first few registers
function h$logCall(c) {
  var f = c;
  if(c && c.n) {
    f = c.n;
  } else {
    f = c.toString().substring(0,20); // h$collectProps(c);
  }
  h$log(h$threadString(h$currentThread) + ":" + h$sp + "  calling: " + f + "    " + JSON.stringify([h$printReg(h$r1), h$printReg(h$r2), h$printReg(h$r3), h$printReg(h$r4), h$printReg(h$r5)]));
  h$checkStack(c);
}
#endif

var h$threadIdN = 0;

// all threads except h$currentThread
// that are not finished/died can be found here
var h$threads = new h$Queue();
var h$blocked = new h$Set();

/** @constructor */
function h$Thread() {
    this.tid = ++h$threadIdN;
    this.status = THREAD_RUNNING;
    this.stack = [h$done, 0, h$ghczminternalZCGHCziInternalziConcziSynczireportError, h$catch_e];
#ifdef GHCJS_DEBUG_STACK
    this.stack = new Proxy(this.stack, {
     set(obj,prop,value) {
       if (value === undefined || value === null) {
          throw new Error("setting stack offset " + prop + " to " + value);
       }
       else {
         return Reflect.set(...arguments);
       }
     }
    });
#endif
    this.sp = 3;
    this.mask = 0;                // async exceptions masked (0 unmasked, 1: uninterruptible, 2: interruptible)
    this.interruptible = false;   // currently in an interruptible operation
    this.excep = [];              // async exceptions waiting for unmask of this thread
    this.delayed = false;         // waiting for threadDelay
    this.blockedOn = null;        // object on which thread is blocked
    this.retryInterrupted = null; // how to retry blocking operation when interrupted
    this.transaction = null;      // for STM
    this.noPreemption  = false;
    this.isSynchronous = false;
    this.continueAsync = false;
    this.m = 0;                     // gc mark
    this.result            = null;  // result value (used for GHCJS.Foreign.Callback)
    this.resultIsException = false;
#ifdef GHCJS_PROF
    this.ccs = h$CCS_SYSTEM;      // cost-centre stack
#endif
    this._key = this.tid;         // for storing in h$Set / h$Map
#ifdef GHCJS_DEBUG_ALLOC
    h$debugAlloc_notifyAlloc(this);
#endif
}

function h$rts_getThreadId(t) { // returns a CULLong
  RETURN_UBX_TUP2((t.tid / Math.pow(2,32))>>>0, (t.tid & 0xFFFFFFFF)>>>0);
}

function h$cmp_thread(t1,t2) {
  if(t1.tid < t2.tid) return -1;
  if(t1.tid > t2.tid) return 1;
  return 0;
}

// description of the thread, if unlabeled then just the thread id
function h$threadString(t) {
  if(t === null) {
    return "<no thread>";
  } else if(t.label) {
    var str = h$decodeUtf8z(t.label, 0);
    return str + " (" + t.tid + ")";
  } else {
    return (""+t.tid);
  }
}

function h$getThreadLabel(t) {
  if (t.label) {
    RETURN_UBX_TUP2(1, t.label);
  } else {
    RETURN_UBX_TUP2(0, 0);
  }
}

function h$listThreads() {
  var r = h$newArray(0,null);
  var t;

  if (h$currentThread) r.push(h$currentThread);

  var threads_iter = h$threads.iter();
  while ((t = threads_iter()) !== null) r.push(t);

  var blocked_iter = h$blocked.iter();
  while ((t = blocked_iter.next()) !== null) r.push(t);

  return r;
}

function h$fork(a, inherit) {
  h$r1 = h$forkThread(a, inherit);
  return h$yield();
}

function h$forkThread(a, inherit) {
  var t = new h$Thread();
  TRACE_SCHEDULER("sched: forking: " + h$threadString(t))
  if(inherit && h$currentThread) {
    t.mask = h$currentThread.mask;
  }
#ifdef GHCJS_PROF
  t.ccs = h$CCS_MAIN;
#endif
  // TRACE_SCHEDULER("sched: action forked: " + a.f.n)
  t.stack[4] = h$ap_1_0;
  t.stack[5] = a;
  t.stack[6] = h$return;
  t.sp = 6;
  h$wakeupThread(t);
  return t;
}

function h$threadStatus(t) {
  // status, capability, locked
  RETURN_UBX_TUP3(t.status, 0, 1);
}

// Required by Google Closure Compiler static code analysis
var h$fds = {};

// Copied from GHCJS because it is required by Google Closure Compiler
// static code analysis
function h$fdReady(fd, write, msecs, isSock) {
  var f = h$fds[fd];
  if(write) {
    if(f.writeReady) {
      return 1;
    } else if(msecs === 0) {
      return 0;
    } else {
      throw "h$fdReady: blocking not implemented";
    }
  } else {
    if(f.readReady) {
      return 1;
    } else if(msecs === 0) {
      return 0;
    } else {
      throw "h$fdReady: blocking not implemented";
    }
  }
}

function h$waitRead(fd) {
  h$fds[fd].waitRead.push(h$currentThread);
  h$currentThread.interruptible = true;
  return h$blockThread(h$currentThread,fd,[h$waitRead,fd]);
}

function h$waitWrite(fd) {
  h$fds[fd].waitWrite.push(h$currentThread);
  h$currentThread.interruptible = true;
  return h$blockThread(h$currentThread,fd,[h$waitWrite,fd]);
}

// threadDelay support:
var h$delayed = new h$HeapSet();
function h$wakeupDelayed(now) {
    while(h$delayed.size() > 0 && h$delayed.peekPrio() < now) {
        var t = h$delayed.pop();
        TRACE_SCHEDULER("delay timeout expired: " + t.tid)
        // might have been woken up early, don't wake up again in that case
        if(t.delayed) {
            t.delayed = false;
            h$wakeupThread(t);
        }
    }
}

function h$delayThread(time) {
  var ms = time/1000; // we have no microseconds in JS
  return h$delayThreadMs(ms);
}

function h$sleep(secs) {
  return h$delayThreadMs(secs*1000);
}

function h$delayThreadMs(ms) {
  var now = Date.now();
  TRACE_SCHEDULER("delaying " + h$threadString(h$currentThread) + " " + ms + "ms (" + (now+ms) + ")")
  h$delayed.add(now+ms, h$currentThread);
  h$currentThread.delayed = true;
  h$currentThread.interruptible = true;
  return h$blockThread(h$currentThread, h$delayed,[h$resumeDelayThread]);
}

function h$resumeDelayThread() {
  h$r1 = false;
  return h$rs(); // stack[h$sp];
}

function h$yield() {
  if(h$currentThread.isSynchronous) {
    return h$stack[h$sp];
  } else {
    h$sp += 2;
    h$stack[h$sp-1] = h$r1;
    h$stack[h$sp] = h$return;
    h$currentThread.sp = h$sp;
    return h$reschedule;
  }
}

// raise the async exception in the thread if not masked
function h$killThread(t, ex) {
  TRACE_SCHEDULER("killThread: " + h$threadString(t))
  if(t === h$currentThread) {
    // if target is self, then throw even if masked
    h$sp += 2;
    h$stack[h$sp-1] = h$r1;
    h$stack[h$sp]   = h$return;
    return h$throw(ex,true);
  } else {
    TRACE_SCHEDULER("killThread mask: " + t.mask)
    if(t.mask === 0 || (t.mask === 2 && t.interruptible)) {
      if(t.stack) {  // finished threads don't have a stack anymore
        h$forceWakeupThread(t);
        t.sp += 2;
        t.stack[t.sp-1] = ex;
        t.stack[t.sp] = h$raiseAsync_frame;
      }
      return h$stack ? h$stack[h$sp] : null;
    } else {
      t.excep.push([h$currentThread,ex]);
      if(h$currentThread) {
        h$currentThread.interruptible = true;
        h$sp += 2;
        h$stack[h$sp-1] = h$r1;
        h$stack[h$sp] = h$return;
        return h$blockThread(h$currentThread,t,null);
      } else {
        return null;
      }
    }
  }
}

function h$maskStatus() {
  TRACE_SCHEDULER("mask status: " + h$currentThread.mask)
  return h$currentThread.mask;
}

function h$maskAsync(a) {
  TRACE_SCHEDULER("mask: thread " + h$threadString(h$currentThread))
  if(h$currentThread.mask !== 2) {
    if(h$currentThread.mask === 0 && h$stack[h$sp] !== h$maskFrame && h$stack[h$sp] !== h$maskUnintFrame) {
      h$stack[++h$sp] = h$unmaskFrame;
    }
    if(h$currentThread.mask === 1) {
      h$stack[++h$sp] = h$maskUnintFrame;
    }
    h$currentThread.mask = 2;
  }
  h$r1 = a;
  return h$ap_1_0_fast();
}

function h$maskUnintAsync(a) {
  TRACE_SCHEDULER("mask unint: thread " + h$threadString(h$currentThread))
  if(h$currentThread.mask !== 1) {
    if(h$currentThread.mask === 2) {
      h$stack[++h$sp] = h$maskFrame;
    } else {
      h$stack[++h$sp] = h$unmaskFrame;
    }
    h$currentThread.mask = 1;
  }
  h$r1 = a;
  return h$ap_1_0_fast();
}

function h$unmaskAsync(a) {
  TRACE_SCHEDULER("unmask: " + h$threadString(h$currentThread))
  if(h$currentThread.excep.length > 0) {
    h$currentThread.mask = 0;
    h$sp += 3;
    h$stack[h$sp-2] = h$ap_1_0;
    h$stack[h$sp-1] = a;
    h$stack[h$sp]   = h$return;
    return h$reschedule;
  }
  if(h$currentThread.mask !== 0) {
    if(h$stack[h$sp] !== h$unmaskFrame) {
      if(h$currentThread.mask === 2) {
        h$stack[++h$sp] = h$maskFrame;
      } else {
        h$stack[++h$sp] = h$maskUnintFrame;
      }
    }
    h$currentThread.mask = 0;
  }
  h$r1 = a;
  return h$ap_1_0_fast();
}

function h$pendingAsync() {
  var t = h$currentThread;
  return (t.excep.length > 0 && (t.mask === 0 || (t.mask === 2 && t.interruptible)));
}

// post the first of the queued async exceptions to
// this thread, restore frame is in thread if alreadySuspended

function h$postAsync(alreadySuspended,next) {
    var t = h$currentThread;
    var v = t.excep.shift();
    TRACE_SCHEDULER("posting async to " + h$threadString(t) + " mask status: " + t.mask + " remaining exceptions: " + t.excep.length)
    var tposter = v[0]; // posting thread, blocked
    var ex      = v[1]; // the exception
    if(v !== null && tposter !== null) {
        h$wakeupThread(tposter);
    }
    if(!alreadySuspended) {
        h$suspendCurrentThread(next);
    }
    h$sp += 2;
    h$stack[h$sp-1]    = ex;
    h$stack[h$sp]      = h$raiseAsync_frame;
    t.sp = h$sp;
}

// wakeup thread, thread has already been removed
// from any queues it was blocked on
function h$wakeupThread(t) {
    TRACE_SCHEDULER("sched: waking up: " + h$threadString(t))
    if(t.status === THREAD_BLOCKED) {
        t.blockedOn = null;
        t.status = THREAD_RUNNING;
        h$blocked.remove(t);
    }
    t.interruptible = false;
    t.retryInterrupted = null;
    h$threads.enqueue(t);
    h$startMainLoop();
}

// force wakeup, remove this thread from any
// queue it's blocked on
function h$forceWakeupThread(t) {
  TRACE_SCHEDULER("forcing wakeup of: " + h$threadString(t))
  if(t.status === THREAD_BLOCKED) {
    h$removeThreadBlock(t);
    h$wakeupThread(t);
  }
}

function h$removeThreadBlock(t) {
  var i;
  if(t.status === THREAD_BLOCKED) {
    var o = t.blockedOn;
    if(o === null || o === undefined) {
      throw ("h$removeThreadBlock: blocked on null or undefined: " + h$threadString(t));
    } else if(o === h$delayed) {
      // thread delayed
      h$delayed.remove(t);
      t.delayed = false;
    } else if(o instanceof h$MVar) {
      TRACE_SCHEDULER("blocked on MVar")
      TRACE_SCHEDULER("MVar before: " + o.readers.length() + " " + o.writers.length() + " " + o.waiters.length)
      // fixme this is rather inefficient
      var r, rq = new h$Queue();
      while((r = o.readers.dequeue()) !== null) {
          if(r !== t) rq.enqueue(r);
      }
      var w, wq = new h$Queue();
      while ((w = o.writers.dequeue()) !== null) {
        if(w[0] !== t) wq.enqueue(w);
      }
      o.readers = rq;
      o.writers = wq;
      if(o.waiters) {
        var wa = [], wat;
        for(i=0;i<o.waiters.length;i++) {
          wat = o.waiters[i];
          if(wat !== t) wa.push(wat);
        }
        o.waiters = wa;
      }
      TRACE_SCHEDULER("MVar after: " + o.readers.length() + " " + o.writers.length() + " " + o.waiters.length)
/*    } else if(o instanceof h$Fd) {
      TRACE_SCHEDULER("blocked on fd")
      h$removeFromArray(o.waitRead,t);
      h$removeFromArray(o.waitWrite,t); */
    } else if(o instanceof h$Thread) {
      TRACE_SCHEDULER("blocked on async exception")
      // set thread (first in pair) to null, exception will still be delivered
      // but this thread not woken up again
      // fixme: are these the correct semantics?
      for(i=0;i<o.excep.length;i++) {
        if(o.excep[i][0] === t) {
          o.excep[i][0] = null;
          break;
        }
      }
    } else if (o instanceof h$TVarsWaiting) {
      h$stmRemoveBlockedThread(o, t)
    } else if(IS_BLACKHOLE(o)) {
      TRACE_SCHEDULER("blocked on blackhole")
      h$removeFromArray(BLACKHOLE_QUEUE(o),t);
    } else {
      throw ("h$removeThreadBlock: blocked on unknown object: " + h$collectProps(o));
    }
    if(t.retryInterrupted) {
      t.sp+=2;
      t.stack[t.sp-1] = t.retryInterrupted;
      t.stack[t.sp] = h$retryInterrupted;
    }
  }
}

function h$removeFromArray(a,o) {
  var i;
  while((i = a.indexOf(o)) !== -1) {
    a.splice(i,1);
  }
}

function h$finishThread(t) {
    TRACE_SCHEDULER("sched: finishing: " + h$threadString(t))
    t.status = THREAD_FINISHED;
    h$blocked.remove(t);
    t.stack = null;
    t.mask = 0;
    for(var i=0;i<t.excep.length;i++) {
        var v = t.excep[i];
        var tposter = v[0];
        if(v !== null && tposter !== null) {
            h$wakeupThread(tposter);
        }
    }
    t.excep = [];
}

function h$blockThread(t,o,resume) {
    TRACE_SCHEDULER("sched: blocking: " + h$threadString(t))
    if(t !== h$currentThread) {
        throw "h$blockThread: blocked thread is not the current thread";
    }
    if(o === undefined || o === null) {
        throw ("h$blockThread, no block object: " + h$threadString(t));
    }
    t.status = THREAD_BLOCKED;
    t.blockedOn = o;
    t.retryInterrupted = resume;
    t.sp = h$sp;
    h$blocked.add(t);
    return h$reschedule;
}

// the main scheduler, called from h$mainLoop
// returns null if nothing to do, otherwise
// the next function to run
var h$lastGc = Date.now();
var h$gcInterval = GHCJS_GC_INTERVAL; // ms
function h$scheduler(next) {
    TRACE_SCHEDULER("sched: scheduler: " + h$sp)
    // if we have a running synchronous thread, the only thing we can do is continue
    if(h$currentThread &&
       h$currentThread.isSynchronous &&
       h$currentThread.status === THREAD_RUNNING) {
        return next;
    }
    var now = Date.now();
    h$wakeupDelayed(now);
    // find the next runnable thread in the run queue
    // remove non-runnable threads
    if(h$currentThread && h$pendingAsync()) {
        TRACE_SCHEDULER("sched: received async exception, continuing thread")
        if(h$currentThread.status !== THREAD_RUNNING) {
            h$forceWakeupThread(h$currentThread);
            h$currentThread.status = THREAD_RUNNING;
        }
        h$postAsync(next === h$reschedule, next);
        return h$stack[h$sp];
    }
    var t;
    while(t = h$threads.dequeue()) {
        if(t.status === THREAD_RUNNING) { break; }
    }
    // if no other runnable threads, just continue current (if runnable)
    if(t === null) {
        TRACE_SCHEDULER("sched: no other runnable threads")
        if(h$currentThread && h$currentThread.status === THREAD_RUNNING) {
            // do gc after a while
            if(now - h$lastGc > h$gcInterval) {
                // save active data for the thread on its stack
                if(next !== h$reschedule && next !== null) {
                    h$suspendCurrentThread(next);
                    next = h$stack[h$sp];
                }
                var ct = h$currentThread;
                h$currentThread = null;
#ifdef GHCJS_PROF
                h$reportCurrentCcs();
#endif
                h$gc(ct);
                h$currentThread = ct;
#ifdef GHCJS_PROF
                h$reportCurrentCcs();
#endif
                // gc might replace the stack of a thread, so reload it
                h$stack = h$currentThread.stack;
                h$sp    = h$currentThread.sp
            }
            TRACE_SCHEDULER("sched: continuing: " + h$threadString(h$currentThread))
            return (next===h$reschedule || next === null)?h$stack[h$sp]:next; // just continue
        } else {
            TRACE_SCHEDULER("sched: pausing")
            h$currentThread = null;
#ifdef GHCJS_PROF
            h$reportCurrentCcs();
#endif
            // We could set a timer here so we do a gc even if Haskell pauses for a long time.
            // However, currently this isn't necessary because h$mainLoop always sets a timer
            // before it pauses.
            if(now - h$lastGc > h$gcInterval)
                h$gc(null);
            return null; // pause the haskell runner
        }
    } else { // runnable thread in t, switch to it
        TRACE_SCHEDULER("sched: switching to: " + h$threadString(t))
        if(h$currentThread !== null) {
            if(h$currentThread.status === THREAD_RUNNING) {
                h$threads.enqueue(h$currentThread);
            }
            // if h$reschedule called, thread takes care of suspend
            if(next !== h$reschedule && next !== null) {
                TRACE_SCHEDULER("sched: suspending: " + h$threadString(h$currentThread))
                // suspend thread: push h$restoreThread stack frame
                h$suspendCurrentThread(next);
            } else {
                TRACE_SCHEDULER("sched: no suspend needed, reschedule called from: " + h$threadString(h$currentThread))
                h$currentThread.sp = h$sp;
            }
            if(h$pendingAsync()) h$postAsync(true, next);
        } else {
            TRACE_SCHEDULER("sched: no suspend needed, no running thread")
        }
        // gc if needed
        if(now - h$lastGc > h$gcInterval) {
            h$currentThread = null;
#ifdef GHCJS_PROF
            h$reportCurrentCcs();
#endif
            h$gc(t);
        }
        // schedule new one
        h$currentThread = t;
        h$stack = t.stack;
        h$sp = t.sp;
#ifdef GHCJS_PROF
        h$reportCurrentCcs();
#endif
        TRACE_SCHEDULER("sched: scheduling " + h$threadString(t) + " sp: " + h$sp)
        // TRACE_SCHEDULER("sp thing: " + h$stack[h$sp].n)
        // h$dumpStackTop(h$stack,0,h$sp);
        return h$stack[h$sp];
    }
}

function h$scheduleMainLoop() {
    TRACE_SCHEDULER("scheduling next main loop wakeup")
    if(h$mainLoopImmediate) return;
    h$clearScheduleMainLoop();
    if(h$delayed.size() === 0) {
#ifndef GHCJS_BROWSER
        if(typeof setTimeout !== 'undefined') {
#endif
            TRACE_SCHEDULER("scheduling main loop wakeup in " + h$gcInterval + "ms")
            h$mainLoopTimeout = setTimeout(h$mainLoop, h$gcInterval);
#ifndef GHCJS_BROWSER
        }
#endif
        return;
    }
    var now = Date.now();
    var delay = Math.min(Math.max(h$delayed.peekPrio()-now, 0), h$gcInterval);
#ifndef GHCJS_BROWSER
    if(typeof setTimeout !== 'undefined') {
#endif
        if(delay >= 1) {
            TRACE_SCHEDULER("scheduling main loop wakeup in " + delay + "ms")
            // node.js 0.10.30 has trouble with non-integral delays
            h$mainLoopTimeout = setTimeout(h$mainLoop, Math.round(delay));
        } else {
            h$mainLoopImmediate = h$setImmediate(h$mainLoop);
        }
#ifndef GHCJS_BROWSER
    }
#endif
}

var h$animationFrameMainLoop = false;
#ifdef GHCJS_ANIMATIONFRAME_MAINLOOP
h$animationFrameMainLoop = true;
#endif

function h$clearScheduleMainLoop() {
    if(h$mainLoopTimeout) {
        clearTimeout(h$mainLoopTimeout);
        h$mainLoopTimeout = null;
    }
    if(h$mainLoopImmediate) {
        h$clearImmediate(h$mainLoopImmediate);
        h$mainLoopImmediate = null;
    }
    if(h$mainLoopFrame) {
        cancelAnimationFrame(h$mainLoopFrame);
        h$mainLoopFrame = null;
    }
}

var h$setImmediate, h$clearImmediate;
if(typeof setImmediate !== 'undefined') {
  h$setImmediate = function(f) { return setImmediate(f); }
  h$clearImmediate = function(h) { clearImmediate(h); }
} else {
  h$setImmediate = function(f) { return setTimeout(f, 0); }
  h$clearImmediate = function(h) { clearTimeout(h); }
}

function h$startMainLoop() {
    TRACE_SCHEDULER("start main loop: " + h$running)
    if(h$running) return;
#ifndef GHCJS_BROWSER
    if(typeof setTimeout !== 'undefined') {
#endif
        if(!h$mainLoopImmediate) {
            h$clearScheduleMainLoop();
            h$mainLoopImmediate = h$setImmediate(h$mainLoop);
        }
#ifndef GHCJS_BROWSER
    } else {
      while(true) {
        // the try/catch block appears to prevent a crash with
        // Safari on iOS 10, even though this path is never taken
        // in a browser.
        try {
          h$mainLoop();
        } catch(e) {
          throw e;
        }
      }
    }
#endif
}

#if defined(GHCJS_TRACE_CALLS) || defined(GHCJS_TRACE_STACK)
var h$traceCallsTicks = 0;
#ifndef GHCJS_TRACE_CALLS_DELAY
#define GHCJS_TRACE_CALLS_DELAY 0
#endif
var h$traceCallsDelay = GHCJS_TRACE_CALLS_DELAY;
#endif

var h$busyYield    = GHCJS_BUSY_YIELD;
var h$schedQuantum = GHCJS_SCHED_QUANTUM;

var h$mainLoopImmediate = null; // immediate id if main loop has been scheduled immediately
var h$mainLoopTimeout = null;   // timeout id if main loop has been scheduled with a timeout
var h$mainLoopFrame = null;   // timeout id if main loop has been scheduled with an animation frame
var h$running = false;
var h$nextThread = null;
function h$mainLoop() {
#ifdef GHCJS_PROF
    h$runProf(h$actualMainLoop);
}
function h$actualMainLoop() {
#endif
  if(h$running) return;
  h$clearScheduleMainLoop();
  if(h$currentThread) {
    h$scheduleMainLoop();
    return;
  }
  h$running = true;
  h$runInitStatic();
  h$currentThread = h$nextThread;
#ifdef GHCJS_PROF
  h$reportCurrentCcs();
#endif
  if(h$nextThread !== null) {
    h$stack = h$currentThread.stack;
    h$sp    = h$currentThread.sp;
  }
  var c = null;
  var start = Date.now();
  do {
    c = h$scheduler(c);
    if(c === null) { // no running threads
      h$nextThread = null;
      h$running = false;
      h$currentThread = null;
#ifdef GHCJS_PROF
      h$reportCurrentCcs();
#endif
      h$scheduleMainLoop();
      return;
    }
    // yield to js after h$busyYield (default value GHCJS_BUSY_YIELD)
    if(!h$currentThread.isSynchronous && Date.now() - start > h$busyYield) {
      TRACE_SCHEDULER("yielding to js")
      if(c !== h$reschedule) h$suspendCurrentThread(c);
      h$nextThread = h$currentThread;
      h$currentThread = null;
#ifdef GHCJS_PROF
      h$reportCurrentCcs();
#endif
      h$running = false;
      if(h$animationFrameMainLoop) {
        h$mainLoopFrame = requestAnimationFrame(h$mainLoop);
      } else {
        h$mainLoopImmediate = h$setImmediate(h$mainLoop);
      }
      return;
    }
#ifdef GHCJS_NO_CATCH_MAINLOOP
    // for debugging purposes only, may leave threads in inconsistent state!
    c = h$runThreadSlice(c);
#else
    c = h$runThreadSliceCatch(c);
#endif
  } while(true);
}

function h$runThreadSliceCatch(c) {
  try {
    return h$runThreadSlice(c);
  } catch(e) {
    // uncaught exception in haskell code, kill thread
#ifdef GHCJS_PROF
    h$reportCurrentCcs();
#endif
    c = null;
    if(h$stack && h$stack[0] === h$doneMain_e) {
      h$stack = null;
      h$reportMainLoopException(e, true);
      h$doneMain_e();
    } else {
      h$stack = null;
      h$reportMainLoopException(e, false);
    }
    h$finishThread(h$currentThread);
    h$currentThread.status = THREAD_DIED;
    h$currentThread = null;
  }
  return h$reschedule;
}

/*
  run thread h$currentThread for a single time slice

     - c: the next function to call from the trampoline

  returns:
    the next function to call in this thread

  preconditions:
    h$currentThread is the thread to run
    h$stack         is the stack of this thread
    h$sp            is the stack pointer

    any global variables needed to pass arguments have been set
    the caller has to update the thread state object
 */
function h$runThreadSlice(c) {
  var count, scheduled = Date.now();
  while(c !== h$reschedule &&
        (h$currentThread.noPreemption || h$currentThread.isSynchronous ||
         (Date.now() - scheduled < h$schedQuantum))) {
    count = 0;
    while(c !== h$reschedule && ++count < GHCJS_SCHED_CHECK) {
#if defined(GHCJS_TRACE_CALLS) || defined(GHCJS_TRACE_STACK)
      h$traceCallsTicks++;
      if(h$traceCallsTicks % 1000000 === 0) h$log("ticks: " + h$traceCallsTicks);
#endif
#ifdef GHCJS_TRACE_CALLS
      if(h$traceCallsDelay >= 0 && h$traceCallsTicks >= h$traceCallsDelay) h$logCall(c);
#endif
#ifdef GHCJS_TRACE_STACK
      if(h$traceCallsDelay >= 0 && h$traceCallsTicks >= h$traceCallsDelay) h$logStack(c);
#endif
      c = c();
#if !defined(GHCJS_TRACE_CALLS) && !defined(GHCJS_TRACE_STACK) && !defined(GHCJS_SCHED_DEBUG)
      c = c();
      c = c();
      c = c();
      c = c();
      c = c();
      c = c();
      c = c();
      c = c();
      c = c();
#endif
    }
    if(c === h$reschedule &&
       (h$currentThread.noPreemption || h$currentThread.isSynchronous) &&
       h$currentThread.status === THREAD_BLOCKED) {
      c = h$handleBlockedSyncThread(c);
    }
  }
  return c;
}

function h$reportMainLoopException(e, isMainThread) {
  if(e instanceof h$ThreadAbortedError) return;
  var main = isMainThread ? " main" : "";
  h$log("uncaught exception in Haskell" + main + " thread: " + e.toString());
  if(e.stack) h$log(e.stack);
  if (h$isNode()) {
    process.exit(1);
  }
}


function h$handleBlockedSyncThread(c) {
  TRACE_SCHEDULER("handling blocked sync thread")
  /*
    if we have a blocked synchronous/non-preemptible thread,
    and it's blocked on a black hole, first try to clear
    it.
   */
  var bo = h$currentThread.blockedOn;
  if(h$currentThread.status === THREAD_BLOCKED &&
     IS_BLACKHOLE(bo) &&
     h$runBlackholeThreadSync(bo)) {
    TRACE_SCHEDULER("blackhole succesfully removed")
    c = h$stack[h$sp];
  }
  /*
    if still blocked, then either fall back to async,
    or throw a WouldBlock exception
   */
  if(h$currentThread.isSynchronous && h$currentThread.status === THREAD_BLOCKED) {
    if(h$currentThread.continueAsync) {
      h$currentThread.isSynchronous = false;
      h$currentThread.continueAsync = false;
    } else if(h$currentThread.isSynchronous) {
      TRACE_SCHEDULER("blocking synchronous thread: exception")
      h$sp += 2;
      h$currentThread.sp = h$sp;
      h$stack[h$sp-1] = h$ghczminternalZCGHCziInternalziJSziPrimziInternalziwouldBlock;
      h$stack[h$sp]   = h$raiseAsync_frame;
      h$forceWakeupThread(h$currentThread);
      c = h$raiseAsync_frame;
    } // otherwise a non-preemptible thread, keep it in the same state
  }
  return c;
}

// run the supplied IO action in a new thread
// returns immediately, thread is started in background
function h$run(a) {
  TRACE_SCHEDULER("sched: starting thread")
  var t = h$forkThread(a, false);
  h$startMainLoop();
  return t;
}

/** @constructor */
function h$WouldBlock() {

}

h$WouldBlock.prototype.toString = function() {
  return "Haskell Operation would block";
}

/** @constructor */
function h$HaskellException(msg) {
  this._msg = msg;
}

h$HaskellException.prototype.toString = function() {
  return this._msg;
}

function h$setCurrentThreadResultWouldBlock() {
  h$currentThread.result = new h$WouldBlock();
  h$currentThread.resultIsException = true;
}

function h$setCurrentThreadResultJSException(e) {
  h$currentThread.result = e;
  h$currentThread.resultIsException = true;
}

function h$setCurrentThreadResultHaskellException(msg) {
  h$currentThread.result = new h$HaskellException(msg);
  h$currentThread.resultIsException = true;
}

function h$setCurrentThreadResultValue(v) {
  h$currentThread.result = v;
  h$currentThread.resultIsException = false;
}

/*
   run a Haskell (IO JSVal) action synchronously, returning
   the result. Uncaught Haskell exceptions are thrown as a
   h$HaskellException. If the action could not finish due to
   blocking, a h$WouldBlock exception is thrown instead.

     - a:    the (IO JSVal) action
     - cont: continue async if blocked
         (the call to h$runSyncReturn would still throw h$WouldBlock,
          since there would be no return value)

   returns: the result of the IO action
 */
function h$runSyncReturn(a, cont) {
  var t = new h$Thread();
  TRACE_SCHEDULER("h$runSyncReturn created thread: " + h$threadString(t))
  var aa = MK_AP1(h$ghczminternalZCGHCziInternalziJSziPrimziInternalzisetCurrentThreadResultValue, a);
  h$runSyncAction(t, aa, cont);
  if(t.status === THREAD_FINISHED) {
    if(t.resultIsException) {
      throw t.result;
    } else {
      return t.result;
    }
  } else if(t.status === THREAD_BLOCKED) {
    throw new h$WouldBlock();
  } else {
    throw new Error("h$runSyncReturn: Unexpected thread status: " + t.status);
  }
}

/*
   run a Haskell IO action synchronously, ignoring the result
   or any exception in the Haskell code

     - a:    the IO action
     - cont: continue async if blocked

   returns: true if the action ran to completion, false otherwise

   throws: any uncaught Haskell or JS exception except WouldBlock
 */
function h$runSync(a, cont) {
  var t = new h$Thread();
  TRACE_SCHEDULER("h$runSync created thread: " + h$threadString(t))
  h$runSyncAction(t, a, cont);
  if(t.resultIsException) {
    if(t.result instanceof h$WouldBlock) {
      return false;
    } else {
      throw t.result;
    }
  }
  return t.status === THREAD_FINISHED;
}

function h$runSyncAction(t, a, cont) {
  h$runInitStatic();
  var c = h$return;
  t.stack[2] = h$ghczminternalZCGHCziInternalziJSziPrimziInternalzisetCurrentThreadResultException;
  t.stack[4] = h$ap_1_0;
  t.stack[5] = a;
  t.stack[6] = h$return;
  t.sp = 6;
  t.status = THREAD_RUNNING;
#ifdef GHCJS_PROF
  // fixme this looks wrong
  // t.ccs = h$currentThread.ccs; // TODO: not sure about this
#endif
  t.isSynchronous = true;
  t.continueAsync = cont;
  var ct = h$currentThread;
  var csp = h$sp;
  var cr1 = h$r1; // do we need to save more than this?
  var caught = false, excep = null;
  h$currentThread = t;
  h$stack = t.stack;
  h$sp = t.sp;
#ifdef GHCJS_PROF
  h$reportCurrentCcs();
#endif
  try {
    c = h$runThreadSlice(c);
    if(c !== h$reschedule) {
      throw new Error("h$runSyncAction: h$reschedule expected");
    }
  } catch(e) {
    h$finishThread(h$currentThread);
    h$currentThread.status = THREAD_DIED;
    caught = true;
    excep = e;
  }
  if(ct !== null) {
    h$currentThread = ct;
    h$stack = ct.stack;
    h$sp = csp;
    h$r1 = cr1;
  } else {
    h$currentThread = null;
    h$stack = null;
  }
#ifdef GHCJS_PROF
  // fixme?
  h$reportCurrentCcs();
#endif
  if(t.status !== THREAD_FINISHED && !cont) {
    h$removeThreadBlock(t);
    h$finishThread(t);
  }
  if(caught) throw excep;
}

// run other threads synchronously until the blackhole is 'freed'
// returns true for success, false for failure, a thread blocks
function h$runBlackholeThreadSync(bh) {
  TRACE_SCHEDULER("trying to remove black hole")
  var ct = h$currentThread;
  var sp = h$sp;
  var success = false;
  var bhs = [];
  var currentBh = bh;
  // we don't handle async exceptions here,
  // don't run threads with pending exceptions
  if(BLACKHOLE_TID(bh).excep.length > 0) {
    TRACE_SCHEDULER("aborting due to queued async exceptions")
    return false;
  }
  h$currentThread = BLACKHOLE_TID(bh);
  h$stack = h$currentThread.stack;
  h$sp = h$currentThread.sp;
#ifdef GHCJS_PROF
  h$reportCurrentCcs();
#endif
  var c = (h$currentThread.status === THREAD_RUNNING)?h$stack[h$sp]:h$reschedule;
  TRACE_SCHEDULER("switched thread status running: " + (h$currentThread.status === THREAD_RUNNING))
  try {
    while(true) {
      while(c !== h$reschedule && IS_BLACKHOLE(currentBh)) {
        c = c();
        c = c();
        c = c();
        c = c();
        c = c();
      }
      if(c === h$reschedule) {
	// perhaps new blackhole, then continue with that thread,
	// otherwise fail
        if(IS_BLACKHOLE(h$currentThread.blockedOn)) {
          TRACE_SCHEDULER("following another black hole")
          bhs.push(currentBh);
          currentBh = h$currentThread.blockedOn;
          h$currentThread = BLACKHOLE_TID(h$currentThread.blockedOn);
          if(h$currentThread.excep.length > 0) {
            break;
          }
          h$stack = h$currentThread.stack;
          h$sp = h$currentThread.sp;
#ifdef GHCJS_PROF
          h$reportCurrentCcs();
#endif
          c = (h$currentThread.status === THREAD_RUNNING)?h$stack[h$sp]:h$reschedule;
        } else {
          TRACE_SCHEDULER("thread blocked on something that's not a black hole, failing")
          break;
        }
      } else { // blackhole updated: suspend thread and pick up the old one
        TRACE_SCHEDULER("blackhole updated, switching back (" + h$sp + ")")
        TRACE_SCHEDULER("next: " + c.toString())
        h$suspendCurrentThread(c);
        if(bhs.length > 0) {
          TRACE_SCHEDULER("to next black hole")
          currentBh = bhs.pop();
          h$currentThread = BLACKHOLE_TID(currentBh);
          h$stack = h$currentThread.stack;
          h$sp = h$currentThread.sp;
#ifdef GHCJS_PROF
          h$reportCurrentCcs();
#endif
        } else {
          TRACE_SCHEDULER("last blackhole removed, success!")
          success = true;
          break;
        }
      }
    }
  } catch(e) { }
  // switch back to original thread
  h$sp = sp;
  h$stack = ct.stack;
  h$currentThread = ct;
#ifdef GHCJS_PROF
  h$reportCurrentCcs();
#endif
  return success;
}

function h$syncThreadState(tid) {
  return (tid.isSynchronous ? 1 : 0) |
    ((tid.continueAsync || !tid.isSynchronous) ? 2 : 0) |
    ((tid.noPreemption || tid.isSynchronous) ? 4 : 0);
}

// run the supplied IO action in a main thread
// (program exits when this thread finishes)
function h$main(a) {
  var t = new h$Thread();
#ifdef GHCJS_PROF
  t.ccs = a.cc;
#endif
  //TRACE_SCHEDULER("sched: starting main thread")
    t.stack[0] = h$doneMain_e;
#ifndef GHCJS_BROWSER
  if(!h$isBrowser() && !h$isGHCJSi()) {
    t.stack[2] = h$ghczminternalZCGHCziInternalziTopHandlerzitopHandler;
  }
#endif
  t.stack[4] = h$ap_1_0;
  t.stack[5] = h$flushStdout;
  t.stack[6] = h$return;
  t.stack[7] = h$ap_1_0;
  t.stack[8] = a;
  t.stack[9] = h$return;
  t.sp = 9;
  t.label = h$encodeUtf8("main");
  h$wakeupThread(t);
  h$startMainLoop();
  return t;
}

function h$doneMain() {
#ifndef GHCJS_BROWSER
  if(h$isGHCJSi()) {
    if(h$currentThread.stack) {
      global.h$GHCJSi.done(h$currentThread);
    }
  } else {
#endif
    h$exitProcess(0);
#ifndef GHCJS_BROWSER
  }
#endif
  h$finishThread(h$currentThread);
  return h$reschedule;
}

/** @constructor */
function h$ThreadAbortedError(code) {
  this.code = code;
}

h$ThreadAbortedError.prototype.toString = function() {
  return "Thread aborted, exit code: " + this.code;
}

function h$exitProcess(code) {
#ifndef GHCJS_BROWSER
    if(h$isNode()) {
	process.exit(code);
    } else if(h$isJvm()) {
        java.lang.System.exit(code);
    } else if(h$isJsShell()) {
        quit(code);
    } else if(h$isJsCore()) {
        if(h$base_stdoutLeftover.val !== null) print(h$base_stdoutLeftover.val);
        if(h$base_stderrLeftover.val !== null) debug(h$base_stderrLeftover.val);
        // jsc does not support returning a nonzero value, print it instead
        if(code !== 0) debug("GHCJS JSC exit status: " + code);
        quit();
    } else {
#endif
        if(h$currentThread) {
            h$finishThread(h$currentThread);
            h$stack = null;
            throw new h$ThreadAbortedError(code);
        }
#ifndef GHCJS_BROWSER
    }
#endif
}

// MVar support
var h$mvarId = 0;
/** @constructor */
function h$MVar() {
  TRACE_SCHEDULER("h$MVar constructor")
  this.val     = null;
  this.readers = new h$Queue();
  this.writers = new h$Queue();
  this.waiters = null;  // waiting for a value in the MVar with ReadMVar
  this.m       = 0; // gc mark
  this.id      = ++h$mvarId;
#ifdef GHCJS_DEBUG_ALLOC
  h$debugAlloc_notifyAlloc(this);
#endif
}

// set the MVar to empty unless there are writers
function h$notifyMVarEmpty(mv) {
  var w = mv.writers.dequeue();
  if(w !== null) {
    var thread = w[0];
    var val    = w[1];
    TRACE_SCHEDULER("notifyMVarEmpty(" + mv.id + "): writer ready: " + h$threadString(thread))
    mv.val = val;
    // thread is null if some JavaScript outside Haskell wrote to the MVar
    if(thread !== null) {
      h$wakeupThread(thread);
    }
  } else {
    TRACE_SCHEDULER("notifyMVarEmpty(" + mv.id + "): no writers")
    mv.val = null;
  }
  TRACE_SCHEDULER("notifyMVarEmpty(" + mv.id + "): " + mv.val)
}

// set the MVar to val unless there are readers
function h$notifyMVarFull(mv,val) {
  if(mv.waiters && mv.waiters.length > 0) {
    for(var i=0;i<mv.waiters.length;i++) {
      var w = mv.waiters[i];
      TRACE_SCHEDULER("notifyMVarFull: notifying waiter: " + h$threadString(w))
      w.sp += 2;
      w.stack[w.sp-1] = val;
      w.stack[w.sp]   = h$return;
      h$wakeupThread(w);
    }
    mv.waiters = null;
  }
  var r = mv.readers.dequeue();
  if(r !== null) {
    TRACE_SCHEDULER("notifyMVarFull(" + mv.id + "): reader ready: " + h$threadString(r))
    r.sp += 2;
    r.stack[r.sp-1] = val;
    r.stack[r.sp]   = h$return;
    h$wakeupThread(r);
    mv.val = null;
  } else {
    TRACE_SCHEDULER("notifyMVarFull(" + mv.id + "): no readers")
    mv.val = val;
  }
  TRACE_SCHEDULER("notifyMVarFull(" + mv.id + "): " + mv.val)
}

function h$takeMVar(mv) {
  TRACE_SCHEDULER("h$takeMVar(" + mv.id + "): " + mv.val + " " + h$threadString(h$currentThread))
  if(mv.val !== null) {
    h$r1 = mv.val;
    h$notifyMVarEmpty(mv);
    return h$stack[h$sp];
  } else {
    mv.readers.enqueue(h$currentThread);
    h$currentThread.interruptible = true;
    return h$blockThread(h$currentThread,mv,[h$takeMVar,mv]);
  }
}

function h$tryTakeMVar(mv) {
  TRACE_SCHEDULER("h$tryTakeMVar(" + mv.id + "): " + mv.val)
  if(mv.val === null) {
    RETURN_UBX_TUP2(0, null);
  } else {
    var v = mv.val;
    h$notifyMVarEmpty(mv);
    RETURN_UBX_TUP2(1, v);
  }
}

function h$readMVar(mv) {
  TRACE_SCHEDULER("h$readMVar(" + mv.id + "): " + mv.val)
  if(mv.val === null) {
    if(mv.waiters) {
      mv.waiters.push(h$currentThread);
    } else {
      mv.waiters = [h$currentThread];
    }
    h$currentThread.interruptible = true;
    return h$blockThread(h$currentThread,mv,[h$readMVar,mv]);
  } else {
    h$r1 = mv.val;
    return h$stack[h$sp];
  }
}

function h$putMVar(mv,val) {
  TRACE_SCHEDULER("h$putMVar(" + mv.id + "): " + mv.val)
  if(mv.val !== null) {
    mv.writers.enqueue([h$currentThread,val]);
    h$currentThread.interruptible = true;
    return h$blockThread(h$currentThread,mv,[h$putMVar,mv,val]);
  } else {
    h$notifyMVarFull(mv,val);
    return h$stack[h$sp];
  }
}

function h$tryPutMVar(mv,val) {
  TRACE_SCHEDULER("h$tryPutMVar(" + mv.id + "): " + mv.val)
  if(mv.val !== null) {
    return 0;
  } else {
    h$notifyMVarFull(mv,val);
    return 1;
  }
}

// box up a JavaScript value and write it to the MVar synchronously
function h$writeMVarJs1(mv,val) {
  var v = MK_DATA1_1(val);
  if(mv.val !== null) {
    TRACE_SCHEDULER("h$writeMVarJs1: was full")
    mv.writers.enqueue([null,v]);
  } else {
    TRACE_SCHEDULER("h$writeMVarJs1: was empty")
    h$notifyMVarFull(mv,v);
  }
}

function h$writeMVarJs2(mv,val1,val2) {
  var v = MK_DATA1_2(val1, val2);
  if(mv.val !== null) {
    TRACE_SCHEDULER("h$writeMVarJs2: was full")
    mv.writers.enqueue([null,v]);
  } else {
    TRACE_SCHEDULER("h$writeMVarJs2: was empty")
    h$notifyMVarFull(mv,v);
  }
}

// IORef support
/** @constructor */
function h$MutVar(v) {
    this.val = v;
    this.m = 0;
#ifdef GHCJS_DEBUG_ALLOC
    h$debugAlloc_notifyAlloc(this);
#endif
}

function h$atomicModifyMutVar(mv, fun) {
  var oldVal = mv.val;
  var thunk = MK_AP1(fun, oldVal);
  mv.val = thunk;
  RETURN_UBX_TUP2(oldVal, thunk);
}

function h$atomicModifyMutVar2(mv, fun) {
  var oldVal = mv.val;
  var thunk = MK_AP1(fun, oldVal);
  mv.val = MK_SELECT1(thunk);
  RETURN_UBX_TUP2(oldVal, thunk);
}

// Black holes and updates
// caller must save registers on stack
function h$blockOnBlackhole(c) {
  TRACE_SCHEDULER("blackhole, blocking: " + h$collectProps(c))
  if(BLACKHOLE_TID(c) === h$currentThread) {
    TRACE_SCHEDULER("NonTermination")
    return h$throw(h$ghczminternalZCGHCziInternalziControlziExceptionziBasezinonTermination, true);
  }
  TRACE_SCHEDULER("blackhole, blocking thread: " + h$threadString(h$currentThread))
  if(BLACKHOLE_QUEUE(c) === null) {
    SET_BLACKHOLE_QUEUE(c,[h$currentThread]);
  } else {
    BLACKHOLE_QUEUE(c).push(h$currentThread);
  }
  return h$blockThread(h$currentThread,c,[h$resumeBlockOnBlackhole,c]);
}

function h$resumeBlockOnBlackhole(c) {
  h$r1 = c;
  return h$ap_0_0_fast();
}

// async exception happened in a black hole, make a thunk
// to resume the computation
// var h$debugResumableId = 0;
function h$makeResumable(bh,start,end,extra) {
  var s = h$stack.slice(start,end+1);
  if(extra) {
    s = s.concat(extra);
  }
//  TRACE_SCHEDULER("making resumable " + (h$debugResumableId+1) + ", stack: ")
//  h$dumpStackTop(s,0,s.length-1);
  MAKE_RESUMABLE(bh, s);
}

var h$enabled_capabilities = h$newByteArray(4);
h$enabled_capabilities.i3[0] = 1;

function h$rtsSupportsBoundThreads() {
  return 0;
}

function h$rts_setMainThread(t) {

}

// async foreign calls
function h$mkForeignCallback(x) {
    return function() {
        if(x.mv === null) { // callback called synchronously
            x.mv = arguments;
        } else {
            h$notifyMVarFull(x.mv, MK_DATA1_1(arguments));
            h$mainLoop();
        }
    }
}

// event listeners through MVar
function h$makeMVarListener(mv, stopProp, stopImmProp, preventDefault) {
  var f = function(event) {
    TRACE_SCHEDULER("MVar listener callback")
    h$writeMVarJs1(mv,event);
    if(stopProp) { event.stopPropagation(); }
    if(stopImmProp) { event.stopImmediatePropagation(); }
    if(preventDefault) { event.preventDefault(); }
  }
  f.root = mv;
  return f;
}

function h$rs() {
  return h$stack[h$sp];
}
