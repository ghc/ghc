#include "Rts.h"

// See Note [threadDelay on wasm] for details.

HsBool rts_JSFFI_flag = HS_BOOL_FALSE;

HsBool rts_JSFFI_used(void);
HsBool rts_JSFFI_used(void) {
  return rts_JSFFI_flag;
}

HsStablePtr rts_threadDelay_impl = NULL;

HsStablePtr rts_threadDelay_sp(void);
HsStablePtr rts_threadDelay_sp(void) {
  return rts_threadDelay_impl;
}
