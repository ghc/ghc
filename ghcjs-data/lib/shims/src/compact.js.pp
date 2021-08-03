#include <ghcjs/rts.h>

#ifdef GHCJS_TRACE_COMPACT
function h$logCompact() { h$log.apply(h$log,arguments); }
#define TRACE_COMPACT(args...) h$logCompact(args)
#else
#define TRACE_COMPACT(args...)
#endif

function h$compactNew(size) {
  TRACE_COMPACT("compactNew" + size);
  throw new Error("not implemented");
}

function h$compactResize(compact, size) {
  TRACE_COMPACT("compactResize" + size);
}

function h$compactContains(compact, obj) {
  TRACE_COMPACT("compactContains");
  return 0;
}

function h$compactContainsAny(obj) {
  TRACE_COMPACT("compactContainsAny");
  return 0;
}

function h$compactGetFirstBlock(compact) {
  TRACE_COMPACT("compactGetFirstBlock");
  RETURN_UBX_TUP2(null, 0);
}

function h$compactGetNextBlock(compact, blocka, blokco) {
  TRACE_COMPACT("compactGetNextBlock");
  RETURN_UBX_TUP2(null, 0);
}

function h$compactAllocateBlock(size, suggesta, suggesto) {
  TRACE_COMPACT("compactAllocateBlock" + size);
  throw new Error("not implemented");
  // returns new root address
  RETURN_UBX_TUP2(null, 0);
}

function h$compactFixupPointers(blocka, blocko, roota, rooto) {
  TRACE_COMPACT("compactFixupPointers");
  throw new Error("not implemented");
  // returns new root address
  RETURN_UBX_TUP2(null, 0);
}


function h$compactAdd(compact, obj) {
  TRACE_COMPACT("compactAdd");
  throw new Error("not implemented");
}


function h$compactAddWithSharing(compact, obj) {
  TRACE_COMPACT("compactAddWithSharing");
  throw new Error("not implemented");
}


function h$compactCompactSize(compact) {
  TRACE_COMPACT("compactSize");
  return 0;
}
