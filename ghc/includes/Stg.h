/* -----------------------------------------------------------------------------
 * $Id: Stg.h,v 1.9 1999/03/15 16:30:25 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Top-level include file for everything STG-ish.  
 *
 * This file is included *automatically* by all .hc files.
 *
 * ---------------------------------------------------------------------------*/

#ifndef STG_H
#define STG_H

#ifndef NON_POSIX_SOURCE
#define _POSIX_SOURCE
#endif

/* Configuration */
#include "config.h"
#ifdef __HUGS__ /* vile hack till the GHC folks come on board */
#include "options.h"
#endif

/* ToDo: Set this flag properly: COMPILER and INTERPRETER should not be mutually exclusive. */
#ifndef INTERPRETER
#define COMPILER 1
#endif

/* This is a feature test - doesn't belong here. FixMe. */
#ifdef __MINGW32__
#define HAVE_WIN32_DLL_SUPPORT
#endif

#ifdef HAVE_WIN32_DLL_SUPPORT
# if __GNUC__ && !defined(__declspec)
#  define DLLIMPORT
# else
#  define DLLIMPORT __declspec(dllimport)
#  define DLLIMPORT_DATA(x) _imp__##x
# endif
#else
# define DLLIMPORT
#endif

#ifdef COMPILING_RTS
#define DLL_IMPORT DLLIMPORT
#define DLL_IMPORT_RTS
#define DLL_IMPORT_DATA
#define DLL_IMPORT_DATA_VAR(x) x
#else
#define DLL_IMPORT
#define DLL_IMPORT_RTS DLLIMPORT
#define DLL_IMPORT_DATA DLLIMPORT
# ifdef HAVE_WIN32_DLL_SUPPORT
#  define DLL_IMPORT_DATA_VAR(x) _imp__##x
# else
#  define DLL_IMPORT_DATA_VAR(x) x
# endif
#endif

/* bit macros
 */
#define BITS_PER_BYTE 8
#define BITS_IN(x) (BITS_PER_BYTE * sizeof(x))

/* -----------------------------------------------------------------------------
   Assertions and Debuggery
   -------------------------------------------------------------------------- */

#ifndef DEBUG
#define ASSERT(predicate) /* nothing */
#else

void _stgAssert (char *, unsigned int);

#define ASSERT(predicate)			\
	if (predicate)				\
	    /*null*/;				\
	else					\
	    _stgAssert(__FILE__, __LINE__)
#endif /* DEBUG */

/* -----------------------------------------------------------------------------
   Include everything STG-ish
   -------------------------------------------------------------------------- */

/* Global type definitions*/
#include "StgTypes.h"

/* Global constaints */
#include "Constants.h"

/* Profiling information */
#include "Profiling.h"

/* Storage format definitions */
#include "Closures.h"
#include "ClosureTypes.h"
#include "InfoTables.h"
#include "TSO.h"

/* STG/Optimised-C related stuff */
#include "MachRegs.h"
#include "Regs.h"
#include "TailCalls.h"

/* these are all ANSI C headers */
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <errno.h>
#include <stdio.h>

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* GNU mp library */
#include "gmp.h"

/* Wired-in Prelude identifiers */
#include "Prelude.h"

/* Storage Manager */
#include "StgStorage.h"

/* Macros for STG/C code */
#include "ClosureMacros.h"
#include "InfoMacros.h"
#include "StgMacros.h"
#include "StgProf.h"
#include "PrimOps.h"
#include "Updates.h"
#include "StgTicky.h"
#include "CCall.h"
#include "Stable.h"

/* Built-in entry points */
#include "StgMiscClosures.h"

/* Runtime-system hooks */
#include "Hooks.h"

/* Misc stuff without a home */
#ifdef BUILDING_RTS_DLL
extern DLLIMPORT char **prog_argv;	/* so we can get at these from Haskell */
extern DLLIMPORT int    prog_argc;
#else
extern char **prog_argv;	/* so we can get at these from Haskell */
extern int    prog_argc;
#endif

extern char **environ;

/* Creating and destroying an adjustor thunk.
   I cannot make myself create a separate .h file
   for these two (sof.)
*/
extern void* createAdjustor(int cconv, StgStablePtr hptr, StgFunPtr wptr);
extern void  freeHaskellFunctionPtr(void* ptr);

#endif /* STG_H */
