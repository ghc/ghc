/* -----------------------------------------------------------------------------
 * $Id: Stg.h,v 1.15 1999/07/05 17:25:23 sof Exp $
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

/* Some macros to handle DLLing (Win32 only at the moment). */
#include "StgDLL.h"

/* Turn lazy blackholing and eager blackholing on/off.
 *
 * Using eager blackholing makes things easier to debug because
 * the blackholes are more predictable - but it's slower and less sexy.
 *
 * For now, do lazy and not eager.
 */

#define LAZY_BLACKHOLING
/* #define EAGER_BLACKHOLING */

#ifdef TICKY_TICKY
/* TICKY_TICKY needs EAGER_BLACKHOLING to verify no double-entries of single-entry thunks. */
# undef  LAZY_BLACKHOLING 
# define EAGER_BLACKHOLING
#endif

/* ToDo: Set this flag properly: COMPILER and INTERPRETER should not be mutually exclusive. */
#ifndef INTERPRETER
#define COMPILER 1
#endif

/* TABLES_NEXT_TO_CODE says whether to assume that info tables are
 * assumed to reside just before the code for a function.
 *
 * UNDEFINING THIS WON'T WORK ON ITS OWN.  You have been warned.
 */
#ifndef USE_MINIINTERPRETER
#define TABLES_NEXT_TO_CODE
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

/* RTS public interface */
#include "RtsAPI.h"

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
#if defined(ENABLE_WIN32_DLL_SUPPOT) && !defined(COMPILING_RTS)
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
