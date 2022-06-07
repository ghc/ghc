// software transactional memory

#ifdef GHCJS_TRACE_STM
function h$logStm() { if(arguments.length == 1) {
                         h$log("stm: " + arguments[0]);
                       } else {
                         h$log.apply(h$log,arguments);
                       }
                     }
#define TRACE_STM(args...) h$logStm(args)
#else
#define TRACE_STM(args...)
#endif


var h$stmTransactionActive = 0;
var h$stmTransactionWaiting = 4;
/** @constructor */
function h$Transaction(o, parent) {
    TRACE_STM("h$Transaction: " + o + " -> " + parent);
    this.action        = o;
    // h$TVar -> h$WrittenTVar, transaction-local changed values
    this.tvars         = new h$Map();
    // h$TVar -> h$LocalTVar, all local tvars accessed anywhere in the transaction
    this.accessed      = parent===null?new h$Map():parent.accessed;
    // nonnull while running a check, contains read variables in this part of the transaction
    this.checkRead     = parent===null?null:parent.checkRead;
    this.parent        = parent;
    this.state         = h$stmTransactionActive;
    this.invariants    = []; // invariants added in this transaction
    this.m             = 0;  // gc mark
#ifdef GHCJS_DEBUG_ALLOC
    h$debugAlloc_notifyAlloc(this);
#endif
}

var h$stmInvariantN = 0;
/** @constructor */
function h$StmInvariant(a) {
    this.action = a;
    this._key = ++h$stmInvariantN;
}
/** @constructor */
function h$WrittenTVar(tv,v) {
    this.tvar = tv;
    this.val = v;
}

var h$TVarN = 0;
/** @constructor */
function h$TVar(v) {
    TRACE_STM("creating TVar, value: " + h$collectProps(v));
    this.val        = v;           // current value
    this.blocked    = new h$Set(); // threads that get woken up if this TVar is updated
    this.invariants = null;        // invariants that use this TVar (h$Set)
    this.m          = 0;           // gc mark
    this._key       = ++h$TVarN;   // for storing in h$Map/h$Set
#ifdef GHCJS_DEBUG_ALLOC
    h$debugAlloc_notifyAlloc(this);
#endif
}

/** @constructor */
function h$TVarsWaiting(s) {
  this.tvars = s;  // h$Set of TVars we're waiting on
#ifdef GHCJS_DEBUG_ALLOC
  h$debugAlloc_notifyAlloc(this);
#endif
}

/** @constructor */
function h$LocalInvariant(o) {
  this.action = o;
  this.dependencies = new h$Set();
}

// local view of a TVar
/** @constructor */
function h$LocalTVar(v) {
  TRACE_STM("creating TVar view for: " + h$collectProps(v));
  this.readVal = v.val;  // the value when read from environment
  this.val     = v.val;  // the current uncommitted value
  this.tvar    = v;
}

function h$atomically(o) {
  h$p3(o, h$atomically_e, h$checkInvariants_e);
  return h$stmStartTransaction(o);
}

function h$stmStartTransaction(o) {
  TRACE_STM("starting transaction: " + h$collectProps(o));
  var t = new h$Transaction(o, null);
  h$currentThread.transaction = t;
  h$r1 = o;
  return h$ap_1_0_fast();
}

function h$stmUpdateInvariantDependencies(inv) {
    var ii, iter = h$currentThread.transaction.checkRead.iter();
    if(inv instanceof h$LocalInvariant) {
        while((ii = iter.next()) !== null) inv.dependencies.add(ii);
    } else {
        while((ii = iter.next()) !== null) h$stmAddTVarInvariant(ii, inv);
    }
}

function h$stmAddTVarInvariant(tv, inv) {
    if(tv.invariants === null) tv.invariants = new h$Set();
    tv.invariants.add(inv);
}

// commit current transaction,
// if it's top-level, commit the TVars, otherwise commit to parent
function h$stmCommitTransaction() {
    var t      = h$currentThread.transaction;
    var tvs    = t.tvars;
    var wtv, i = tvs.iter();
    if(t.parent === null) { // top-level commit
        TRACE_STM("committing top-level transaction");
	// write new value to TVars and collect blocked threads
        var thread, threadi, blockedThreads = new h$Set();
        while((wtv = i.nextVal()) !== null) {
	    h$stmCommitTVar(wtv.tvar, wtv.val, blockedThreads);
	}
	// wake up all blocked threads
        threadi = blockedThreads.iter();
        while((thread = threadi.next()) !== null) {
	    h$stmRemoveBlockedThread(thread.blockedOn, thread);
            h$wakeupThread(thread);
	}
	// commit our new invariants
        for(var j=0;j<t.invariants.length;j++) {
            h$stmCommitInvariant(t.invariants[j]);
        }
    } else { // commit subtransaction
        TRACE_STM("committing subtransaction");
        var tpvs = t.parent.tvars;
        while((wtv = i.nextVal()) !== null) tpvs.put(wtv.tvar, wtv);
        t.parent.invariants = t.parent.invariants.concat(t.invariants);
    }
    h$currentThread.transaction = t.parent;
}

function h$stmValidateTransaction() {
    var ltv, i = h$currentThread.transaction.accessed.iter();
    while((ltv = i.nextVal()) !== null) {
        if(ltv.readVal !== ltv.tvar.val) return false;
    }
    return true;
}

function h$stmAbortTransaction() {
  h$currentThread.transaction = h$currentThread.transaction.parent;
}


// add an invariant
function h$stmCheck(o) {
  h$currentThread.transaction.invariants.push(new h$LocalInvariant(o));
  return false;
}

function h$stmRetry() {
  // unwind stack to h$atomically_e or h$stmCatchRetry_e frame
  while(h$sp > 0) {
    var f = h$stack[h$sp];
    if(f === h$atomically_e || f === h$stmCatchRetry_e) {
      break;
    }
    var size;
    if(f === h$ap_gen) {
      size = ((h$stack[h$sp-1] >> 8) + 2);
    } else {
      var tag = f.gtag;
      if(tag < 0) { // dynamic size
        size = h$stack[h$sp-1];
      } else {
        size = (tag & 0xff) + 1;
      }
    }
    h$sp -= size;
  }
  // either h$sp == 0 or at a handler
  if(h$sp > 0) {
    if(f === h$atomically_e) {
      return h$stmSuspendRetry();
    } else { // h$stmCatchRetry_e
      var b = h$stack[h$sp-1];
      h$stmAbortTransaction();
      h$sp -= 2;
      h$r1 = b;
      return h$ap_1_0_fast();
    }
  } else {
    throw "h$stmRetry: STM retry outside a transaction";
  }
}

function h$stmSuspendRetry() {
    var tv, i = h$currentThread.transaction.accessed.iter();
    var tvs = new h$Set();
    while((tv = i.next()) !== null) {
        TRACE_STM("h$stmSuspendRetry, accessed: " + h$collectProps(tv));
        tv.blocked.add(h$currentThread);
        tvs.add(tv);
    }
    var waiting = new h$TVarsWaiting(tvs);
    h$currentThread.interruptible = true;
    h$p2(waiting, h$stmResumeRetry_e);
    return h$blockThread(h$currentThread, waiting);
}

function h$stmCatchRetry(a,b) {
    h$currentThread.transaction = new h$Transaction(b, h$currentThread.transaction);
    h$p2(b, h$stmCatchRetry_e);
    h$r1 = a;
    return h$ap_1_0_fast();
}

function h$catchStm(a,handler) {
    h$p4(h$currentThread.transaction, h$currentThread.mask, handler, h$catchStm_e);
    h$r1 = a;
    return h$ap_1_0_fast();
}

function h$newTVar(v) {
  return new h$TVar(v);
}

function h$readTVar(tv) {
  return h$readLocalTVar(h$currentThread.transaction,tv);
}

function h$readTVarIO(tv) {
  return tv.val;
}

function h$writeTVar(tv, v) {
  h$setLocalTVar(h$currentThread.transaction, tv, v);
}

function h$sameTVar(tv1, tv2) {
  return tv1 === tv2;
}

// get the local value of the TVar in the transaction t
// tvar is added to the read set
function h$readLocalTVar(t, tv) {
  if(t.checkRead !== null) {
    t.checkRead.add(tv);
  }
  var t0 = t;
  while(t0 !== null) {
    var v = t0.tvars.get(tv);
    if(v !== null) {
      TRACE_STM("h$readLocalTVar: found locally modified value: " + h$collectProps(v));
      return v.val;
    }
    t0 = t0.parent;
  }
  var lv = t.accessed.get(tv);
  if(lv !== null) {
    TRACE_STM("h$readLocalTVar: found TVar value: " + h$collectProps(lv));
    return lv.val;
  } else {
    TRACE_STM("h$readLocalTVar: TVar value not found, adding: " + h$collectProps(tv));
    t.accessed.put(tv, new h$LocalTVar(tv));
    return tv.val;
  }
}

function h$setLocalTVar(t, tv, v) {
    if(!t.accessed.has(tv)) t.accessed.put(tv, new h$LocalTVar(tv));
    if(t.tvars.has(tv)) {
        t.tvars.get(tv).val = v;
    } else {
        t.tvars.put(tv, new h$WrittenTVar(tv, v));
    }
}

function h$stmCheckInvariants() {
    var t = h$currentThread.transaction;
    function addCheck(inv) {
        h$p5(inv, h$stmCheckInvariantResult_e, t, inv, h$stmCheckInvariantStart_e);
    }
    h$p2(h$r1, h$return);
    var wtv, i = t.tvars.iter();
    while((wtv = i.nextVal()) !== null) {
        TRACE_STM("h$stmCheckInvariants: checking: " + h$collectProps(wtv));
        var ii = wtv.tvar.invariants;
        if(ii) {
            var iv, iii = ii.iter();
            while((iv = iii.next()) !== null) addCheck(iv);
        }
    }
    for(var j=0;j<t.invariants.length;j++) {
        addCheck(t.invariants[j]);
    }
    return h$stack[h$sp];
}

function h$stmCommitTVar(tv, v, threads) {
    TRACE_STM("committing tvar: " + tv._key + " " + (v === tv.val));
    if(v !== tv.val) {
        var thr, iter = tv.blocked.iter();
        while((thr = iter.next()) !== null) threads.add(thr);
        tv.blocked.clear();
        tv.val = v;
    }
}

// remove the thread from the queues of the TVars in s
function h$stmRemoveBlockedThread(s, thread) {
    var tv, i = s.tvars.iter();
    while((tv = i.next()) !== null) {
        tv.blocked.remove(thread);
    }
}

function h$stmCommitInvariant(localInv) {
    var inv = new h$StmInvariant(localInv.action);
    var dep, i = localInv.dependencies.iter();
    while((dep = i.next()) !== null) {
        h$stmAddTVarInvariant(dep, inv);
    }
}
