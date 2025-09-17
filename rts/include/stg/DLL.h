/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Support for Windows DLLs.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#  define DLL_IMPORT_DATA_REF(x) (&(x))
#  define DLL_IMPORT_DATA_VARNAME(x) x
#  define DLLIMPORT

/* The view of the rts/include/ header files differ ever so
   slightly depending on whether the RTS is being compiled
   or not - so we're forced to distinguish between two.
   [oh, you want details :) : Data symbols defined by the RTS
    have to be accessed through an extra level of indirection
    when compiling generated .hc code compared to when the RTS
    sources are being processed. This is only the case when
    using Win32 DLLs. ]
*/
#if defined(COMPILING_RTS)
#define DLL_IMPORT_RTS
#define DLL_IMPORT_DATA_VAR(x) x
#else
#define DLL_IMPORT_RTS DLLIMPORT
#define DLL_IMPORT_DATA_VAR(x) x
#endif
