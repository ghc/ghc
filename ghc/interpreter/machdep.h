/* -*- mode: hugs-c; -*- */
/*---------------------------------------------------------------------------
 * Interrupting execution (signals, allowBreak):
 *-------------------------------------------------------------------------*/

extern Bool breakOn      Args((Bool));

extern Bool  broken;                    /* indicates interrupt received    */

#ifndef SIGBREAK /* Sigh, not defined in cygwin32 beta release 16 */
# define SIGBREAK 21
#endif

/* allowBreak: call to allow user to interrupt computation
 * ctrlbrk:    set control break handler
 */

#if HUGS_FOR_WINDOWS
#  define ctrlbrk(bh) 
#  define allowBreak()  kbhit()
#else /* !HUGS_FOR_WINDOWS */
#  define ctrlbrk(bh)   signal(SIGINT,bh); signal(SIGBREAK,bh)
#  define allowBreak()  if (broken) { broken=FALSE; sigRaise(breakHandler); }
#endif /* !HUGS_FOR_WINDOWS */

/*---------------------------------------------------------------------------
 * Environment variables and the registry
 *-------------------------------------------------------------------------*/

/* On Win32 we can use the registry to supplement info in environment 
 * variables.
 */
#define USE_REGISTRY (HAVE_WINDOWS_H && !__MSDOS__)

#ifdef USE_REGISTRY
Bool 	writeRegString Args((String var, String val));
String 	readRegString  Args((String var, String def));
Int 	readRegInt     Args((String var, Int def));
Bool 	writeRegInt    Args((String var, Int val));
#endif

/*---------------------------------------------------------------------------
 * File operations:
 *-------------------------------------------------------------------------*/

#if HAVE_UNISTD_H
# include <sys/types.h>
# include <unistd.h>
#elif !HUGS_FOR_WINDOWS
extern int      chdir      Args((const char*));
#endif

#if HAVE_STDLIB_H
# include <stdlib.h>
#else
extern int      system     Args((const char *));
extern double   atof       Args((const char *));
extern void     exit       Args((int));
#endif

#ifndef FILENAME_MAX       /* should already be defined in an ANSI compiler*/
#define FILENAME_MAX 256
#else
#if     FILENAME_MAX < 256
#undef  FILENAME_MAX
#define FILENAME_MAX 256
#endif
#endif

/* Hack, hack: if you have dos.h, you probably have a DOS filesystem */
#define DOS_FILENAMES              HAVE_DOS_H
/* ToDo: can we replace this with a feature test? */
#define MAC_FILENAMES              SYMANTEC_C

#define CASE_INSENSITIVE_FILENAMES (DOS_FILENAMES | RISCOS)

#if CASE_INSENSITIVE_FILENAMES
# if HAVE_STRCASECMP
#  define filenamecmp(s1,s2) strcasecmp(s1,s2)
# elif HAVE__STRICMP
#  define filenamecmp(s1,s2) _stricmp(s1,s2)
# elif HAVE_STRICMP
#  define filenamecmp(s1,s2) stricmp(s1,s2)
# elif HAVE_STRCMPI
#  define filenamecmp(s1,s2) strcmpi(s1,s2)
# endif
#else
# define filenamecmp(s1,s2) strcmp(s1,s2)
#endif

/*---------------------------------------------------------------------------
 * Pipe-related operations:
 *
 * On Windows, many standard Unix names acquire a leading underscore.
 * Irritating, but easy to work around.
 *-------------------------------------------------------------------------*/

#if !defined(HAVE_POPEN) && defined(HAVE__POPEN)
#define popen(x,y) _popen(x,y)
#endif
#if !defined(HAVE_PCLOSE) && defined(HAVE__PCLOSE)
#define pclose(x) _pclose(x)
#endif

/*---------------------------------------------------------------------------
 * Bit manipulation:
 *-------------------------------------------------------------------------*/

#define bitArraySize(n)    ((n)/bitsPerWord + 1)
#define placeInSet(n)      ((-(n)-1)>>wordShift)
#define maskInSet(n)       (1<<((-(n)-1)&wordMask))

/*---------------------------------------------------------------------------
 * Function prototypes for code in machdep.c
 *-------------------------------------------------------------------------*/

#if RISCOS
typedef struct { unsigned hi, lo; } Time;
#define timeChanged(now,thn)    (now.hi!=thn.hi || now.lo!=thn.lo)
#define timeSet(var,tm)         var.hi = tm.hi; var.lo = tm.lo
#else
typedef time_t Time;
#define timeChanged(now,thn)    (now!=thn)
#define timeSet(var,tm)         var = tm
#endif

extern Void   getFileInfo      Args((String, Time *, Long *));
extern int    pathCmp          Args((String, String));
extern String substPath        Args((String,String));
extern Bool   startEdit        Args((Int,String));

extern  String findPathname     Args((String,String));
extern  String findMPathname    Args((String,String));

extern  Int    shellEsc         Args((String));
extern  Int    getTerminalWidth Args((Void));
extern  Void   normalTerminal   Args((Void));
extern  Void   noechoTerminal   Args((Void));
extern  Int    readTerminalChar Args((Void));
extern  Void   gcStarted        Args((Void));
extern  Void   gcScanning       Args((Void));
extern  Void   gcRecovered      Args((Int));
extern  Void   gcCStack         Args((Void));

/*-------------------------------------------------------------------------*/
