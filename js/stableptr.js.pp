/*
  Stable pointers are all allocated in the h$StablePtrData buffer and
  can therefore be distinguished by offset

  StablePtr# is treated as Word32# when it comes to writing and reading them
 */

 #ifdef GHCJS_TRACE_STABLEPTR
 function h$logStablePtr(args) { h$log.apply(h$log,arguments); }
 #define TRACE_STABLEPTR(args...) h$logStablePtr(args)
 #else
 #define TRACE_STABLEPTR(args...)
 #endif

var h$stablePtrData = [null];
var h$stablePtrBuf  = h$newByteArray(8);
var h$stablePtrN    = 1;
var h$stablePtrFree = [];

function h$makeStablePtr(v) {
  TRACE_STABLEPTR("makeStablePtr");
  if(!v) return 0;
  var slot = h$stablePtrFree.pop();
  if(slot === undefined) {
    slot = h$stablePtrN++;
  }
  TRACE_STABLEPTR("  -> slot:" + slot);
  h$stablePtrData[slot] = v;
  return slot << 2;
}

var h$foreignExports = [];
function h$foreignExport(f, packageName, moduleName, functionName, typeSig) {
  h$foreignExports.push({ exported: f,
                          package: packageName,
                          mod:  moduleName,
                          name: functionName,
                          sig: typeSig
                        });
  // console.log("foreign export:", f, packageName, moduleName, functionName, typeSig);
  h$makeStablePtr(f);
  if(typeof exports === 'object') {
    if(typeof exports[functionName] === 'undefined') {
      exports[functionName] = f;
    }
  }
}
/*
function h$foreignExportStablePtr(x) {
  // h$makeStablePtr(x);
}
*/
function h$deRefStablePtr(stable_o) {
  var slot = stable_o >> 2;
  return h$stablePtrData[slot];
}

function h$hs_free_stable_ptr(stable_d, stable_o) {
  var slot = stable_o >> 2;
  TRACE_STABLEPTR("hs_free_stable_ptr");
  if(h$stablePtrData[slot] !== null) {
    h$stablePtrData[slot] = null;
    h$stablePtrFree.push(slot);
  }
}

// not strictly stableptr, but we make it work only for stable pointers
function h$addrToAny(addr_v, addr_o) {
  TRACE_STABLEPTR("addrToAny");
  TRACE_STABLEPTR(addr_v === h$stablePtrBuf);
  var slot = addr_o >> 2;
  return h$stablePtrData[slot];
}
