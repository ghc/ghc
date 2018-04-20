/* -----------------------------------------------------------------------------
*
* (c) The GHC Team 1998-2000
*
* Hides indirection for EH handlers for different platforms
*
* ---------------------------------------------------------------------------*/

#pragma once

#include "ghcconfig.h"

// On windows Excn provides two macros
// BEGIN_WINDOWS_VEH_HANDLER  and END_WINDOWS_VEH_HANDLER, which
// will catch such exceptions in the entire process and die by
// printing a message and calling stg_exit(1).
//
// For other operating systems an empty macro is defined so
// that no #ifdefs are needed around the usage of these macros.


#if defined(mingw32_HOST_OS)
#include "win32/veh_excn.h"

#define BEGIN_WINDOWS_VEH_HANDLER __register_hs_exception_handler();
#define END_WINDOWS_VEH_HANDLER __unregister_hs_exception_handler();
#else
#define BEGIN_WINDOWS_VEH_HANDLER
#define END_WINDOWS_VEH_HANDLER
#endif /* mingw32_HOST_OS */
