/* --------------------------------------------------------------------------
 * Connections between components of the Hugs system
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: connect.h,v $
 * $Revision: 1.26 $
 * $Date: 2000/03/10 17:30:36 $
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Standard data:
 * ------------------------------------------------------------------------*/

extern Bool   haskell98;                /* TRUE => Haskell 98 compatibility*/
extern Bool   combined;                 /* TRUE => combined operation      */
extern Module modulePrelude;

/* --------------------------------------------------------------------------
 * Primitive constructor functions 
 * ------------------------------------------------------------------------*/

extern Name  nameFalse, nameTrue;
extern Name  nameNil,   nameCons;
extern Name  nameJust,  nameNothing;
extern Name  nameLeft,  nameRight;
extern Name  nameUnit;

extern Name  nameLT,      nameEQ;
extern Name  nameGT;
extern Name  nameFst,     nameSnd;      /* standard combinators            */
extern Name  nameId,      nameOtherwise;
extern Name  nameNegate,  nameFlip;     /* primitives reqd for parsing     */
extern Name  nameFrom,    nameFromThen;
extern Name  nameFromTo,  nameFromThenTo;
extern Name  nameFatbar,  nameFail;     /* primitives reqd for translation */
extern Name  nameIf,      nameSel;
extern Name  nameCompAux;
extern Name  namePmInt,   namePmFlt;    /* primitives for pattern matching */
extern Name  namePmInteger;
extern Name  namePmNpk,   namePmSub;    /* primitives for (n+k) patterns   */
extern Name  nameError;                 /* For runtime error messages      */
extern Name  nameUndefined;             /* A generic undefined value       */
extern Name  nameBlackHole;             /* For GC-detected black hole      */
extern Name  nameInd;                   /* For dict indirection            */
extern Name  nameAnd,     nameOr;       /* For optimisation of && and ||   */
extern Name  nameFromInt, nameFromDouble;/*coercion of numerics            */
extern Name  nameFromInteger;
extern Name  nameEq,      nameCompare;  /* names used for deriving         */
extern Name  nameMinBnd,  nameMaxBnd;
extern Name  nameIndex,   nameInRange;
extern Name  nameRange;
extern Name  nameLe,      nameGt;
extern Name  nameShowsPrec, nameReadsPrec;
extern Name  nameMult,    namePlus;
extern Name  nameComp,    nameApp;      /* composition and append          */
extern Name  nameShowField;             /* display single field            */
extern Name  nameShowParen;             /* wrap with parens                */
extern Name  nameReadField;             /* read single field               */
extern Name  nameReadParen;             /* unwrap from parens              */
extern Name  nameLex;                   /* lexer                           */
extern Name  nameRangeSize;             /* calculate size of index range   */
extern Class classMonad;                /* Monads                          */
extern Name  nameReturn,  nameBind;     /* for translating monad comps     */
extern Name  nameMFail;
extern Name  nameListMonad;             /* builder function for List Monad */
extern Name  namePrint;                 /* printing primitive              */
extern Name  nameCreateAdjThunk;        /* f-x-dyn: create adjustor thunk  */
extern Text  textPrelude;
extern Text  textNum;                   /* used to process default decls   */
extern Text  textCcall;                 /* used to process foreign import  */
extern Text  textStdcall;               /*         ... and foreign export  */
extern Text  textPlus;                  /* Used to recognise n+k patterns  */

#if TREX
extern Name  nameNoRec;                 /* The empty record                */
extern Type  typeNoRow;                 /* The empty row                   */
extern Type  typeRec;                   /* Record formation                */
extern Kind  extKind;                   /* Kind of extension, *->row->row  */
extern Name  nameRecExt;                /* Extend a record                 */
extern Name  nameRecBrk;                /* Break a record                  */
extern Name  nameAddEv;                 /* Addition of evidence values     */
extern Name  nameRecSel;                /* Select a record                 */
extern Name  nameRecShw;                /* Show a record                   */
extern Name  nameShowRecRow;            /* Used to output rows             */
extern Name  nameRecEq;                 /* Compare records                 */
extern Name  nameEqRecRow;              /* Used to compare rows            */
extern Name  nameInsFld;                /* Field insertion routine         */
#endif

extern String repeatStr;                /* Repeat last command string      */
extern String hugsEdit;                 /* String for editor command       */
extern String hugsPath;                 /* String for file search path     */
extern String projectPath;              /* String for project search path  */

extern Type  typeProgIO;		/* For the IO monad, IO a	   */
extern Type  typeArrow;                 /* Builtin type constructors       */
extern Type  typeList;
extern Type  typeUnit;

#define fn(from,to)  ap(ap(typeArrow,from),to)  /* make type: from -> to   */

extern List  stdDefaults;               /* List of standard default types  */

extern Class classEq;                   /* `standard' classes              */
extern Class classOrd;
extern Class classShow;
extern Class classRead;
extern Class classIx;
extern Class classEnum;
extern Class classBounded;

extern Class classReal;                 /* `numeric' classes               */
extern Class classIntegral;
extern Class classRealFrac;
extern Class classRealFloat;
extern Class classFractional;
extern Class classFloating;
extern Class classNum;

extern Cell  *CStackBase;               /* pointer to base of C stack      */

extern List  tyconDefns;                /* list of type constructor defns  */
extern List  typeInDefns;               /* list of synonym restrictions    */
extern List  valDefns;                  /* list of value definitions       */
extern List  classDefns;                /* list of class definitions       */
extern List  instDefns;                 /* list of instance definitions    */
extern List  selDefns;                  /* list of selector lists          */
extern List  genDefns;                  /* list of generated defns         */
extern List  primDefns;                 /* list of primitive definitions   */
extern List  unqualImports;             /* unqualified import list         */
extern List  defaultDefns;              /* default definitions (if any)    */
extern Int   defaultLine;               /* line in which default defs occur*/
extern List  evalDefaults;              /* defaults for evaluator          */
extern Cell  inputExpr;                 /* evaluator input expression      */
extern Cell  inputContext;		/* evaluator input expression      */
extern Addr  inputCode;                 /* Code for compiled input expr    */

extern Int   whnfArgs;                  /* number of args of term in whnf  */
extern Cell  whnfHead;                  /* head of term in whnf            */
extern Int   whnfInt;                   /* integer value of term in whnf   */
extern Float whnfFloat;                 /* float value of term in whnf     */
extern Long  numCells;                  /* number of cells allocated       */
extern Int   numGcs;                    /* number of garbage collections   */
extern Bool  broken;                    /* indicates interrupt received    */
extern Bool  preludeLoaded;             /* TRUE => prelude has been loaded */

extern Bool  gcMessages;                /* TRUE => print GC messages       */
extern Bool  literateScripts;           /* TRUE => default lit scripts     */
extern Bool  literateErrors;            /* TRUE => report errs in lit scrs */
extern Bool  showInstRes;               /* TRUE => show instance resolution */

extern Int   cutoff;                    /* Constraint Cutoff depth         */

#if USE_PREPROCESSOR
extern String preprocessor;             /* preprocessor command            */
#endif

#if DEBUG_CODE
extern Bool  debugCode;                 /* TRUE => print G-code to screen  */
#endif
extern Bool  debugSC;			/* TRUE => print SC to screen  */
extern Bool  kindExpert;                /* TRUE => display kind errors in  */
                                        /*         full detail             */
extern Bool  allowOverlap;              /* TRUE => allow overlapping insts */

/* --------------------------------------------------------------------------
 * Function prototypes etc...
 * ------------------------------------------------------------------------*/

extern Void everybody Args((Int));


#define RESET    1            /* reset subsystem                           */
#define MARK     2            /* mark parts of graph in use by subsystem   */
#define PREPREL  3            /* do startup actions before Prelude loading */
#define POSTPREL 4            /* do startup actions after Prelude loading  */
#define EXIT     5            /* Take action immediately before exit()     */
#define BREAK    6            /* Take action after program break           */
#define GCDONE   7            /* Restore subsystem invariantss after GC    */

/* PREPREL was formerly called INSTALL.  POSTPREL doesn't have an analogy
   in the old Hugs. 
*/


typedef long   Target;
extern  Void   setGoal          Args((String, Target));
extern  Void   soFar            Args((Target));
extern  Void   done             Args((Void));
extern  String fromEnv          Args((String,String));
extern  Bool   chase            Args((List));

extern  Void   storage          Args((Int));

extern  Void   input            Args((Int));
extern  Void   consoleInput     Args((String));
extern  Void   projInput        Args((String));
extern  Void   stringInput      Args((String));
extern  Void   parseScript      Args((String,Long));
extern  Void   parseExp         Args((Void));
#if EXPLAIN_INSTANCE_RESOLUTION
extern  Void   parseContext     Args((Void));
#endif
extern  String readFilename     Args((Void));
extern  String readLine         Args((Void));
extern  Syntax defaultSyntax    Args((Text));
extern  Syntax syntaxOf         Args((Name));
extern  String unlexChar        Args((Char,Char));
extern  Void   printString      Args((String));

extern  Void   substitution     Args((Int));
extern  Void   optimiser        Args((Int));

extern  Void   staticAnalysis   Args((Int));
extern  Void   startModule      Args((Cell));
extern  Void   setExportList    Args((List));
extern  Void   setExports       Args((List));
extern  Void   addQualImport    Args((Text,Text));
extern  Void   addUnqualImport  Args((Text,List));

extern  Void   tyconDefn        Args((Int,Cell,Cell,Cell));
extern  Void   setTypeIns       Args((List));
extern  Void   clearTypeIns     Args((Void));
extern  Type   fullExpand       Args((Type));
extern  Bool   isAmbiguous      Args((Type));
extern  Void   ambigError       Args((Int,String,Cell,Type));
extern  Void   classDefn	Args((Int,Cell,List,List));
extern  Void   instDefn         Args((Int,Cell,Cell));
extern  Void   addTupInst       Args((Class,Int));
#if TREX
extern  Inst   addRecShowInst   Args((Class,Ext));
extern  Inst   addRecEqInst     Args((Class,Ext));
#endif
extern  List   typeVarsIn	Args((Cell,List,List,List));
extern  List   oclose		Args((List,List));
extern  List   zonkTyvarsIn	Args((Type,List));
extern  Type   zonkTyvar	Args((Int));
extern  Type   zonkType		Args((Type,Int));
extern  Void   primDefn         Args((Cell,List,Cell));
extern  Void   defaultDefn      Args((Int,List));
extern  Void   checkExp         Args((Void));
#if EXPLAIN_INSTANCE_RESOLUTION
extern  Void   checkContext	Args((Void));
#endif
extern  Void   checkDefns       Args((Void));
extern  Bool   h98Pred          Args((Bool,Cell));
extern  Cell   h98Context       Args((Bool,List));
extern  Void   h98CheckCtxt     Args((Int,String,Bool,List,Inst));
extern  Void   h98CheckType     Args((Int,String,Cell,Type));
extern  Void   h98DoesntSupport Args((Int,String));

extern  Void   typeChecker      Args((Int));
extern  Type   typeCheckExp     Args((Bool));
extern  Void   typeCheckDefns   Args((Void));
extern  Cell   provePred        Args((Kinds,List,Cell));
extern  List   simpleContext    Args((List,Int));
extern  Cell   rhsExpr          Args((Cell));
extern  Int    rhsLine          Args((Cell));
extern  Bool   isProgType       Args((List,Type));
extern  Cell   superEvid        Args((Cell,Class,Class));
extern  Void   linkPreludeTC    Args((Void));
extern  Void   linkPreludeCM    Args((Void));

extern  Void   compiler         Args((Int));
extern  Void   compileDefns     Args((Void));
extern  Void   compileExp       Args((Void));
extern  Bool   failFree         Args((Cell));
extern  Int    discrArity       Args((Cell));

extern  Addr   codeGen          Args((Name,Int,Cell));
extern  Void   implementCfun    Args((Name,List));
extern  Void   addCfunTable     Args((Tycon));
extern  Name   succCfun         Args((Name));
extern  Name   nextCfun         Args((Name,Name));
extern  Name   cfunByNum        Args((Name,Int));
extern  Void   unwind           Args((Cell));
extern  Void   run              Args((Addr,StackPtr));

extern  Void   eval             Args((Cell));
extern  Cell   evalWithNoError  Args((Cell));
extern  Void   evalFails        Args((StackPtr));

extern  Void   abandon          Args((String,Cell));
extern  Void   outputString     Args((FILE *));
extern  Void   dialogue         Args((Cell));
#define consChar(c) ap(nameCons,mkChar(c))

extern  Int    shellEsc         Args((String));
extern  Int    getTerminalWidth Args((Void));
extern  Void   normalTerminal   Args((Void));
extern  Void   noechoTerminal   Args((Void));
extern  Int    readTerminalChar Args((Void));
extern  Void   gcStarted        Args((Void));
extern  Void   gcScanning       Args((Void));
extern  Void   gcRecovered      Args((Int));
extern  Void   gcCStack         Args((Void));
extern  Void   needPrims        Args((Int)); 
extern  List   calcFunDepsPreds Args((List));
extern  Inst   findInstFor      Args((Cell,Int));
#if MULTI_INST
extern  List   findInstsFor     Args((Cell,Int));
#endif

extern Void ppScripts ( Void );
extern Void ppModules ( Void );

extern Type primType( Int /*AsmMonad*/ monad, String a_kinds, String r_kinds );
#define aVar            mkOffset(0)     /* Simple skeleton for type var    */

/*-------------------------------------------------------------------------*/

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
# if HAVE_SIGPROCMASK
#  include <signal.h>
#  define ctrlbrk(bh)	{ sigset_t mask; \
			  signal(SIGINT,bh); \
			  sigemptyset(&mask); \
			  sigaddset(&mask, SIGINT); \
			  sigprocmask(SIG_UNBLOCK, &mask, NULL); \
			}
# else
#  define ctrlbrk(bh)	signal(SIGINT,bh)
# endif
#if SYMANTEC_C
extern int time_release;
extern int allow_break_count;
# define allowBreak()	if (time_release !=0 && \
			    (++allow_break_count % time_release) == 0) \
			    ProcessEvent();
#else
# define allowBreak()  if (broken) { broken=FALSE; sigRaise(breakHandler); }
#endif
#endif /* !HUGS_FOR_WINDOWS */

/*---------------------------------------------------------------------------
 * Environment variables and the registry
 *-------------------------------------------------------------------------*/

/* On Win32 we can use the registry to supplement info in environment 
 * variables.
 */
/* AJG: Commented out for now for development */
/* #define USE_REGISTRY (HAVE_WINDOWS_H && !__MSDOS__) */

#ifdef USE_REGISTRY
Bool 	writeRegString Args((String var, String val));
String 	readRegString  Args((String var, String def));
Int 	readRegInt     Args((String var, Int def));
Bool 	writeRegInt    Args((String var, Int val));
#endif

#define N_INSTALLDIR 200
extern char installDir[N_INSTALLDIR];

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

extern  String findMPathname    Args((String,String,String));
extern  String findPathname     Args((String,String));

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

extern Type typeInt64;
extern Type typeWord;
extern Type typeFloat;
extern Type typePrimArray;
extern Type typePrimByteArray;
extern Type typeRef;
extern Type typePrimMutableArray;
extern Type typePrimMutableByteArray;
extern Type typeStable;
extern Type typeWeak;
extern Type typeIO;
extern Type typeForeign;
extern Type typeMVar;
extern Type typeThreadId;
extern Type typeException;
extern Type typeIO;
extern Type typeST;

extern  Void   foreignImport    Args((Cell,Text,Pair,Cell,Cell));
extern List  foreignImports;            /* foreign import declarations     */
extern  Void   implementForeignImport Args((Name));
extern  Void   foreignExport   Args((Cell,Text,Cell,Cell,Cell));
extern List  foreignExports;            /* foreign export declarations     */
extern  Void   implementForeignExport Args((Name));

extern List diVars;
extern Int  diNum;

Int     userArity           Args((Name));


extern List    deriveEq            Args((Tycon));
extern List    deriveOrd           Args((Tycon));
extern List    deriveEnum          Args((Tycon));
extern List    deriveIx            Args((Tycon));
extern List    deriveShow          Args((Tycon));
extern List    deriveRead          Args((Cell));
extern List    deriveBounded       Args((Tycon));
extern List    checkPrimDefn       Args((Triple));

extern Bool  typeMatches        Args((Type,Type));
extern  Void   evalExp           Args((Void));
extern  Void   linkControl      Args((Int));
extern  Void   deriveControl    Args((Int));
extern  Void   translateControl Args((Int));
extern  Void   codegen          Args((Int));
extern  Void   machdep          Args((Int));

extern Void linkPrimitiveNames(void);

extern  Kind  starToStar;                /* Type -> Type                    */
extern Type  boundPair;                 /* (mkOffset(0),mkOffset(0))       */
extern        Type typeOrdering;

extern  Type   conToTagType     Args((Tycon));
extern  Type   tagToConType     Args((Tycon));

#define BOGUS(k) (-9000000-(k))

extern Void putChr  Args((Int));
extern Void putStr  Args((String));
extern Void putInt  Args((Int));
extern Void putPtr  Args((Ptr));

extern Void unlexCharConst Args((Cell));
extern FILE *outputStream;             /* current output stream            */
extern Int  outColumn;                 /* current output column number     */

extern Void unlexStrConst  Args((Text));
extern Void unlexVar       Args((Text));
extern Void unlexVarStr    Args((String));
extern List offsetTyvarsIn          Args((Type,List));

extern List cfunSfuns;                  /* List of (Cfun,[SelectorVar])    */

extern Void  interface        Args((Int));

extern Void getFileSize       Args((String, Long *));

extern ZPair readInterface      Args((String,Long));
extern Bool  processInterfaces  Args((Void));
extern void  ifLinkConstrItbl ( Name n );


extern List /* of ZTriple(I_INTERFACE, 
                          Text--name of obj file, 
                          Int--size of obj file) */
             ifaces_outstanding;


extern Void hi_o_namesFromSrcName Args((String,String*,String* oName));
extern Cell parseInterface        Args((String,Long));

extern String getExtraObjectInfo ( String primaryObjectName,
                                   String extraFileName,
                                   Int*   extraFileSize );

extern Name         newDSel             Args((Class,Int));
extern Int          visitClass          Args((Class));

extern Kind  simpleKind         Args((Int));
