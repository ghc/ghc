//#OPTIONS: CPP

// Globals used by GHC

#define GVAR(name,nvar) \
  var h$global_ ## nvar ## _a = null;\
  var h$global_ ## nvar ## _o = null;\
  function name(a,o) {\
    if (!h$global_ ## nvar ## _a) {\
      h$global_ ## nvar ## _a = a;\
      h$global_ ## nvar ## _o = o;\
    }\
    RETURN_UBX_TUP2(h$global_ ## nvar ##_a, h$global_ ## nvar ##_o);\
  }

GVAR(h$getOrSetLibHSghcGlobalHasPprDebug, has_ppr_debug)
GVAR(h$getOrSetLibHSghcGlobalHasNoDebugOutput, has_no_debug_output)
GVAR(h$getOrSetLibHSghcGlobalHasNoStateHack, has_no_state_hack)
GVAR(h$getOrSetLibHSghcFastStringTable, faststring_table)