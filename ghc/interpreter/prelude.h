
/* --------------------------------------------------------------------------
 * Basic data type definitions, prototypes and standard macros including
 * machine dependent variations...
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: prelude.h,v $
 * $Revision: 1.3 $
 * $Date: 1999/02/03 17:08:36 $
 * ------------------------------------------------------------------------*/

#include "config.h"
#include "options.h"
#include <stdio.h>

/*---------------------------------------------------------------------------
 * Most of the configuration code from earlier versions of Hugs has been moved
 * into config.h (which is usually automatically generated).  
 *
 * Most of the configuration code is "feature based".  That is, the 
 * configure script looks to see if a particular feature (or misfeature)
 * is present on the compiler/OS.  
 *
 * A small amount of configuration code is still "system based": it tests
 * flags to determine what kind of compiler/system it's running on - from
 * which it infers what features the compiler/system has.  Use of system
 * based tests generally indicates that we can't remember/figure out
 * what the original problem was and so we can't add an appropriate feature
 * test to the configure script.
 *-------------------------------------------------------------------------*/

#ifdef __RISCOS__ /* Acorn DesktopC running RISCOS2 or 3 */
# define RISCOS 1
#else
# define RISCOS 0
#endif

#if defined __DJGPP__ && __DJGPP__==2
# define DJGPP2 1
#else
# define DJGPP2 0
#endif

#if defined __MSDOS__ && __MSDOS__ && !DJGPP2
# define DOS 1
#else
# define DOS 0
#endif

#if defined _WIN32 | defined __WIN32__
# define IS_WIN32 1
#else
# define IS_WIN32 0
#endif

/*---------------------------------------------------------------------------
 * Platform-dependent settings:
 *-------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
 * Include windows.h and friends:
 *-------------------------------------------------------------------------*/

#if HAVE_WINDOWS_H
#include <windows.h>                    /* Misc. Windows hackery           */
#endif

#if HUGS_FOR_WINDOWS

#if     __MSDOS__
# define INT           int
# define UNSIGNED      unsigned
# define CHAR          char
# define TCHAR         char
# define UCHAR         UNSIGNED CHAR
# define ULONG         unsigned long
# define APIENTRY      PASCAL
# define HUGE          huge
# define LPOFNHOOKPROC FARPROC
# define CMDdata(w,l)  (HIWORD(l))      /* decoding WM_COMMAND message     */
# define CMDitem(w,l)  (w)
# define CMDhwnd(w,l)  ((HWND)(LOWORD(l)))
#else
# define HUGE
# define CMDdata(w,l)  (HIWORD(w))      /* decoding WM_COMMAND message     */
# define CMDitem(w,l)  (LOWORD(w))
# define CMDhwnd(w,l)  ((HWND)(l))
#endif

#include "win-menu.h"
extern char *appName;
extern HWND             hWndText;       /* text output window handle       */
extern HWND             hWndMain;       /* main window handle              */
#include "win-text.h"
#endif


/*---------------------------------------------------------------------------
 * Macros used in declarations:
 *  function prototypes
 *  local/far declarations
 *  HUGS_noreturn/HUGS_unused (prevent spurious warnings)
 *  result type of main
 *  dynamic linking declarations
 *-------------------------------------------------------------------------*/

#if HAVE_PROTOTYPES       /* To enable use of prototypes whenever possible */
#define Args(x) x
#else
#define Args(x) ()
#endif

/* local = prefix for locally defined functions */
/* far   = prefix for far pointers              */
#if DOS
# define local near pascal
#else
# define local
# define far
#endif

#ifdef __GNUC__     /* Avoid spurious warnings                             */
#if __GNUC__ >= 2 && __GNUC_MINOR__ >= 7
#define HUGS_noreturn  __attribute__ ((noreturn))
#define HUGS_unused    __attribute__ ((unused))
#else
#define HUGS_noreturn  
#define HUGS_unused
#endif
#else
#define HUGS_noreturn  
#define HUGS_unused
#endif

/* result type of main function */
/* Hugs 1.01 could be configured to return void on Unix-like systems
 * but I don't think this is necessary.  ADR
 */
#define Main int
#define MainDone() return 0/*NOTUSED*/

/*---------------------------------------------------------------------------
 * String operations:
 *-------------------------------------------------------------------------*/

#if HAVE_STRING_H
# include <string.h>
#else
extern int      strcmp     Args((const char*, const char*));
extern int      strncmp    Args((const char*, const char*, int));
extern char     *strchr    Args((const char*, int));
extern char     *strrchr   Args((const char*, int));
extern size_t   strlen     Args((const char *));
extern char     *strcpy    Args((char *, const char*));
extern char     *strcat    Args((char *, const char*));
#endif
#if HAVE_STRCMP
#define strCompare strcmp
#else /* probably only used for DOS - ADR */
extern  int     stricmp    Args((const char *, const char*));
#define strCompare stricmp
#endif

#if HAVE_CTYPE_H
# include <ctype.h>
#endif
#ifndef isascii
#define  isascii(c)     (((unsigned)(c))<128)
#endif

/*---------------------------------------------------------------------------
 * Memory allocation
 *-------------------------------------------------------------------------*/

#if HAVE_FARCALLOC
# include <alloc.h>
# define farCalloc(n,s) farcalloc((unsigned long)n,(unsigned long)s)
#elif HAVE_VALLOC
# include <stdlib.h>
# include <malloc.h>
# define farCalloc(n,s) (Void *)valloc(((unsigned)n)*((unsigned)s))
#else
# define farCalloc(n,s) (Void *)calloc(((unsigned)n),((unsigned)s))
#endif

/* bison-generated parsers like to have alloca - so try to define it */
#if HAVE__ALLOCA
#include <malloc.h>
#ifndef alloca
#define alloca _alloca
#endif
#endif

/*---------------------------------------------------------------------------
 * Interrupting execution (signals, allowBreak):
 *-------------------------------------------------------------------------*/

#if !DOS && VOID_INT_SIGNALS
# define sigProto(nm)   void nm Args((int))
# define sigRaise(nm)   nm(1)
# define sigHandler(nm) void nm(sig_arg) int sig_arg;
# define sigResume      return
#else
# define sigProto(nm)   int nm Args((Void))
# define sigRaise(nm)   nm()
# define sigHandler(nm) int nm(Void)
# define sigResume      return 1
#endif

/*---------------------------------------------------------------------------
 * Assertions
 *-------------------------------------------------------------------------*/

#if HAVE_ASSERT_H
#include <assert.h>
#else
#define assert(x) doNothing()
#endif

/*---------------------------------------------------------------------------
 * General settings:
 *-------------------------------------------------------------------------*/

#define Void     void   /* older compilers object to: typedef void Void;   */
typedef unsigned Bool;
#define TRUE     1
#define FALSE    0

typedef char           *String;
typedef int             Int;
typedef long            Long;
typedef int             Char;
typedef unsigned int    Unsigned; /* at least 32 bits */
typedef void*           Ptr;
typedef void*           Addr;
typedef void*           HpPtr;

#define FloatImpType       double
#define FloatPro           double
#define FloatFMT           "%.9g"


/* ToDo: this should probably go in dynamic.h - but then
 * storage.h has to include dynamic.h!
 */
#if HAVE_DLFCN_H /* eg LINUX, SOLARIS, ULTRIX */
typedef void* ObjectFile; 
#elif HAVE_DL_H /* eg HPUX */
typedef shl_t ObjectFile;
#elif HAVE_WINDOWS_H && !defined(__MSDOS__)
typedef HINSTANCE ObjectFile;
#else
#warning GHC file loading not available on this machine
#endif

#define doNothing() do { } while (0) /* Null statement */

#ifndef STD_PRELUDE
#if     RISCOS
#define STD_PRELUDE        "prelude"
#else
#define STD_PRELUDE        "Prelude.hs"
#endif
#endif

#if DYN_TABLES                          /* Tables may be alloc'd at runtime*/
#define DECTABLE(tab)      far *tab     /* macros for declaration & defn   */
#define DEFTABLE(tab,sz)   far *tab = 0
#else                                   /* or at compile-time:             */
#define DECTABLE(tab)      tab[]
#define DEFTABLE(tab,sz)   tab[sz]
#endif

/*---------------------------------------------------------------------------
 * Printf-related operations:
 *-------------------------------------------------------------------------*/

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#if !defined(HAVE_SNPRINTF)
extern int snprintf   Args((char*, int, const char*, ...));
#endif

#if !defined(HAVE_VSNPRINTF)
extern int vsnprintf  Args((char*, int, const char*, va_list));
#endif

/*---------------------------------------------------------------------------
 * Compiler output
 * Tweaking this lets us redirect prompts, error messages, etc - but has no
 * effect on output of Haskell programs (which should use hPutStr and friends).
 *-------------------------------------------------------------------------*/

#if REDIRECT_OUTPUT

extern Void   hugsPrintf            Args((const char *, ...));
extern Void   hugsPutchar           Args((int));
extern Void   hugsFlushStdout       Args((Void));
extern Void   hugsEnableOutput      Args((Bool));
extern String hugsClearOutputBuffer Args((Void));
                            
extern Void   hugsFFlush            Args((FILE*));
extern Void   hugsFPrintf           Args((FILE*, const char*, ...));
extern Void   hugsPutc              Args((int, FILE*));

#define Printf               hugsPrintf
#define Putchar              hugsPutchar
#define FlushStdout          hugsFlushStdout
#define EnableOutput         hugsEnableOutput
#define ClearOutputBuffer    hugsClearOutputBuffer

#define FFlush               hugsFFlush
#define FPrintf              hugsFPrintf
#define Putc                 hugsPutc
                             
#else                        
                             
#define Printf               printf
#define Putchar              putchar
#define FlushStdout()        fflush(stdout)
#define EnableOutput(f)      doNothing()
#define ClearOutputBuffer()  0

#define FFlush               fflush
#define FPrintf              fprintf
#define Putc                 putc

#endif

/*-------------------------------------------------------------------------*/
