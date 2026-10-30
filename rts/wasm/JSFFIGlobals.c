#include "Rts.h"

// See Note [threadDelay on wasm] for details.

RTS_PUBLIC HsBool rts_JSFFI_flag = HS_BOOL_FALSE;

RTS_PUBLIC HsBool rts_JSFFI_used(void);
HsBool rts_JSFFI_used(void) {
  return rts_JSFFI_flag;
}

RTS_PUBLIC HsStablePtr rts_threadDelay_impl = NULL;

RTS_PUBLIC HsStablePtr rts_threadDelay_sp(void);
HsStablePtr rts_threadDelay_sp(void) {
  return rts_threadDelay_impl;
}
