#include "Rts.h"

// Only exists to make the stack_frame_sizeW macro available in Haskell code
// (via FFI).
StgWord stackFrameSizeW(StgClosure *frame){
  return stack_frame_sizeW(frame);
}

// Only exists to make the get_itbl macro available in Haskell code (via FFI).
StgInfoTable* getItbl(StgClosure *closure){
  // printObj(closure);
  return get_itbl(closure);
};
