/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Support for Windows DLLs.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef __STGDLL_H__
#define __STGDLL_H__ 1

#if defined(COMPILING_WINDOWS_DLL)
#  if defined(x86_64_HOST_ARCH)
#    define DLL_IMPORT_DATA_REF(x) (__imp_##x)
#    define DLL_IMPORT_DATA_VARNAME(x) *__imp_##x
#  else
#    define DLL_IMPORT_DATA_REF(x) (_imp__##x)
#    define DLL_IMPORT_DATA_VARNAME(x) *_imp__##x
#  endif
#  if __GNUC__ && !defined(__declspec)
#    define DLLIMPORT
#  else
#    define DLLIMPORT __declspec(dllimport)
#    if defined(x86_64_HOST_ARCH)
#      define DLLIMPORT_DATA(x) __imp_##x
#    else
#      define DLLIMPORT_DATA(x) _imp__##x
#    endif
#  endif
#else
#  define DLL_IMPORT_DATA_REF(x) (&(x))
#  define DLL_IMPORT_DATA_VARNAME(x) x
#  define DLLIMPORT
#endif

/* The view of the ghc/includes/ header files differ ever so
   slightly depending on whether the RTS is being compiled
   or not - so we're forced to distinguish between two.
   [oh, you want details :) : Data symbols defined by the RTS
    have to be accessed through an extra level of indirection
    when compiling generated .hc code compared to when the RTS
    sources are being processed. This is only the case when 
    using Win32 DLLs. ]
*/
#ifdef COMPILING_RTS
#define DLL_IMPORT DLLIMPORT
#define DLL_IMPORT_RTS
#define DLL_IMPORT_DATA_VAR(x) x
#else
#define DLL_IMPORT
#define DLL_IMPORT_RTS DLLIMPORT
# if defined(COMPILING_WINDOWS_DLL)
#  if defined(x86_64_HOST_ARCH)
#   define DLL_IMPORT_DATA_VAR(x) __imp_##x
#  else
#   define DLL_IMPORT_DATA_VAR(x) _imp__##x
#  endif
# else
#  define DLL_IMPORT_DATA_VAR(x) x
# endif
#endif


#ifdef COMPILING_STDLIB
#define DLL_IMPORT_STDLIB
#else
#define DLL_IMPORT_STDLIB DLLIMPORT
#endif

#endif /* __STGDLL_H__ */
