
/* --------------------------------------------------------------------------
 * Configuration options
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: options.h,v $
 * $Revision: 1.19 $
 * $Date: 2000/03/09 06:14:38 $
 * ------------------------------------------------------------------------*/


/* --------------------------------------------------------------------------
 * Hugs paths and directories
 * ------------------------------------------------------------------------*/

/* Define this as the default setting of HUGSPATH.                        
 * Value may contain string "{Hugs}" (for which we will substitute the
 * value of HUGSDIR) and should be either colon-separated (Unix)
 * or semicolon-separated (Macintosh, Windows, DOS).  Escape
 * characters in the path string are interpreted according to normal
 * Haskell conventions.
 *
 * This value can be overridden from the command line by setting the
 * HUGSFLAGS environment variable or by storing an appropriate value
 * for HUGSFLAGS in the registry (Win32 only).  In all cases, use a 
 * string of the form -P"...".  
 */
#define HUGSPATH "."

/* The directory name which is substituted for the string "{Hugs}"
 * in a path variable.  This normally points to where the Hugs libraries
 * are installed - ie so that the file HUGSDIR/lib/Prelude.hs exists    
 * Typical values are:                                  
 *    "/usr/local/lib/hugs"                             
 *    "/usr/homes/JFHaskell/hugs"                       
 *    ".."      
 *
 * This value is ignored on Windows and Macintosh versions since
 * it is assumed that the binary is installed in HUGSDIR.
 *
 * This value cannot be overridden from the command line or by using 
 * environment variables.  This isn't quite as limiting as you'd think
 * since you can always choose _not_ to use the {Hugs} variable - however,
 * it's obviously _nicer_ to have it set correctly.
 */
#ifndef HUGSDIR
#define HUGSDIR "."
#endif


/* --------------------------------------------------------------------------
 * User interface options
 * ------------------------------------------------------------------------*/

/* Define if you want to use the "Hugs for Windows" GUI.
 * (Windows 3.1 and compatibles only)
 */
#define HUGS_FOR_WINDOWS 0

/* Define if you want filenames to be converted to normal form by:
 * o replacing relative pathnames with absolute pathnames and
 *   eliminating .. and . where possible.
 * o converting to lower case (only in case-insensitive filesystems)
 */
#define PATH_CANONICALIZATION 0

/* Define if you want the small startup banner.
 */
#define SMALL_BANNER 0

/* Define if you want to be able to redirect stdout/stderr to a buffer.
 * Only necessary for the Hugs server interface (which is used in the
 * Netscape plugin and the standalone evaluator "runhugs"). 
 */
#define REDIRECT_OUTPUT (!HUGS_FOR_WINDOWS)


/* --------------------------------------------------------------------------
 * Making Hugs smaller
 * ------------------------------------------------------------------------*/

/* Define one of these to select overall size of Hugs
 *   SMALL_HUGS     for 16 bit operation on a limited memory PC.
 *   REGULAR_HUGS   for 32 bit operation using largish default table sizes.
 *   LARGE_HUGS     for 32 bit operation using larger default table sizes.
 */
#define SMALL_HUGS   0
#define REGULAR_HUGS 0
#define LARGE_HUGS   1

#define NUM_SYNTAX         100
#define NUM_TUPLES         37
#define NUM_OFFSETS        1024
#define NUM_CHARS          256
#if TREX
#define NUM_EXT            100
#endif
#define CHAR_MASK          0xff

#if     SMALL_HUGS                      /* the McDonalds mentality :-)     */
#define Pick(s,r,l)        s
#endif
#if     REGULAR_HUGS
#define Pick(s,r,l)        r
#endif
#if     LARGE_HUGS
#define Pick(s,r,l)        l
#endif

#define MINIMUMHEAP        Pick(7500,   19000,      19000)
#define MAXIMUMHEAP        Pick(32765,  0,          0)
#define DEFAULTHEAP        Pick(28000,  50000,      350000)

#define NUM_SCRIPTS        Pick(64,     100,        100)
#define NUM_MODULE         NUM_SCRIPTS
#define NUM_TYCON          Pick(60,     160,        400)
#define NUM_NAME           Pick(1000,   2000,       16000)
#define NUM_CLASSES        Pick(30,     40,         80)
#define NUM_INSTS          Pick(200,    300,        600)
#define NUM_TEXT           Pick(12000,  20000,      100000)
#define NUM_TEXTH          Pick(1,      10,         10)
#define NUM_TYVARS         Pick(800,    2000,       4000)
#define NUM_STACK          Pick(1800,   12000,      16000)
#define NUM_DTUPLES        Pick(3,      5,          5)

#define MAXPOSINT          0x7fffffff
#define MINNEGINT          (-MAXPOSINT-1)
#define MAXHUGSWORD        0xffffffffU

#define BIGBASE            Pick(100,    10000,      10000)
#define BIGEXP             Pick(2,      4,          4)

#define minRecovery        Pick(1000,  1000,       1000)
#define bitsPerWord        Pick(16,    32,         32)
#define wordShift          Pick(4,     5,          5)
#define wordMask           Pick(15,    31,         31)

/* Define to force a fixed size (NUM_TYVARS) for the current substitution.
 * Setting this flag places a limit on the maximum complexity of
 * expressions handled by the typechecker.  It is normally turned off
 * but may be required for small machines/configurations.
 */
#define FIXED_SUBST 0 

/* Define this to allocate tables dynamically.
 * This is currently just a memory saving trick, but this may be
 * extended at a later stage to allow at least some of the tables
 * to be extended dynamically at run-time to avoid exhausted space errors.
 */
#define DYN_TABLES SMALL_HUGS

/* Should quantifiers be displayed in error messages.
 * Warning: not consistently used.
 */
#define DISPLAY_QUANTIFIERS 0

/* Flags to determine which raw representations and operations are available
 * Notes:
 * o if you turn everything on, you might end up with more then 256
 *   bytecodes: check the value of i_ccall (the lst bytecode) to check
 * (JRS), 22apr99: I don't think any of the #undef'd ones will work
 * without attention.  However, standard Haskell 98 is supported 
 * is supported without needing them.
 */
#undef  PROVIDE_STABLE
#undef  PROVIDE_FOREIGN
#undef  PROVIDE_WEAK
#undef  PROVIDE_CONCURRENT
#undef  PROVIDE_PTREQUALITY
#undef  PROVIDE_COERCE

#define  PROVIDE_COERCE     1
#define PROVIDE_PTREQUALITY 1

/* Set to 1 to use a non-GMP implementation of integer, in the
   standalone Hugs.  Set to 0 in the combined GHC-Hugs system,
   in which case GNU MP will be used.
*/
#define STANDALONE_INTEGER 1

/* Enable a crude profiler which counts BCO entries, bytes allocated
   and bytecode insns executed on a per-fn basis.  Used for assessing
   the effect of the simplifier/optimiser.
*/
#undef CRUDE_PROFILING


/* Is the default default (Int,Double) or (Integer,Double)?
 */
#define DEFAULT_BIGNUM 1

/* Are things being used in an interactive setting or a batch setting?
 * In an interactive setting, System.exitWith should not call _exit
 * getProgName and getProgArgs need to be handled differently, etc.
 *
 * Warning: this flag is ignored by an awful lot of code.
 */
#define INTERACTIVE

/* Turn bytecode interpreter support on/off.
 */
#define INTERPRETER 1 

/* Turn on debugging output and some sanity checks
 */
/*#define DEBUG*/

/* NB: LAZY_BLACKHOLING has been moved up to Stg.h where both Hugs and GHC can see it,
 * and EAGER_BLACKHOLING has been introduced also.  KSW 1999-01.
 */

/* Turn miniinterpreter on/off.
 * 
 * The mininterpreter is portable but slow - if you turn it off, 
 * you'll probably need to provide some assembly language support
 * for your architecture.
 */
#define USE_MINIINTERPRETER 1

/* Turn registerisation on/off.
 * 
 * If you turn this off, you'll probably need to provide some
 * assembly language support for your architecture.
 */
#define NO_REGS


/* --------------------------------------------------------------------------
 * Fancy features
 * ------------------------------------------------------------------------*/

/* Define if T-REX; Typed Rows and EXtension should be enabled             */
/* Doesn't work in current system - I don't know what the primops do       */
#define TREX 0

/* Define if :xplain should be enabled					   */
#define EXPLAIN_INSTANCE_RESOLUTION 0


/* Define if you want to run Haskell code through a preprocessor
 * 
 * Note that the :reload command doesn't know about any dependencies
 * introduced by using #include so you must :load (not :reload) if
 * you change any #included files (such as configuration files).
 */
#define USE_PREPROCESSOR 1

/* Define if you want to time every evaluation. 
 *
 * Timing is included in the Hugs distribution for the purpose of benchmarking
 * the Hugs interpreter, comparing its performance across a variety of
 * different machines, and with other systems for similar languages.
 *
 * It would be somewhat foolish to try to use the timings produced in this
 * way for any other purpose.  In particular, using timings to compare the
 * performance of different versions of an algorithm is likely to give very
 * misleading results.  The current implementation of Hugs as an interpreter,
 * without any significant optimizations, means that there are much more
 * significant overheads than can be accounted for by small variations in
 * Hugs code.
 */
/* #undef WANT_TIMER */


/* --------------------------------------------------------------------------
 * Desugaring options
 * 
 * These options are mostly used for developing/debugging the system.
 * Since they turn off required parts of the Haskell language, you'll
 * probably need to modify Prelude.hs and the libraries if you change
 * these flags.
 * ------------------------------------------------------------------------*/

/* Define if single-element dictionaries are implemented by newtype.
 * Should be turned on.  Mostly used to make it easier to find which
 * bits of code implement this optimisation and as a way of documenting
 * them.
 */
#define USE_NEWTYPE_FOR_DICTS 1

/* Define if strings should be represented as normal C strings.
 * Note that this doesn't work if the string contains '\0'
 * and makes persistence problematic.
 * Intended as a stop-gap measure until mutable byte arrays are available.
 */
#define USE_ADDR_FOR_STRINGS 1

/* Define to include support for (n+k) patterns. 
 * Warning: many people in the Haskell committee want to remove n+k patterns.
 */
#define NPLUSK 1


/* --------------------------------------------------------------------------
 * Debugging options (intended for use by maintainers)
 * ------------------------------------------------------------------------*/

/* Define if debugging generated bytecodes or the bytecode interpreter     */
#define DEBUG_CODE 1

/* Define if you want to use a low-level printer from within a debugger    */
#define DEBUG_PRINTER 1

/* --------------------------------------------------------------------------
 * Experimental features
 * These are likely to disappear/change in future versions and should not
 * be used by most people..
 * ------------------------------------------------------------------------*/

/* In a plain Hugs system, most signals (SIGBUS, SIGTERM, etc) indicate
 * some kind of error in Hugs - or maybe a stack overflow.  Rather than
 * just crash, Hugs catches these errors and returns to the main loop.
 * It does this by calling a function "panic" which longjmp's back to the
 * main loop.
 * If you're developing a GreenCard library, this may not be the right
 * behaviour - it's better if Hugs leaves them for your debugger to
 * catch rather than trapping them and "panicking".
 */
#define DONT_PANIC 1


/* ----------------------------------------------------------------------- */
