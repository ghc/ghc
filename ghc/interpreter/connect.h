
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
 * $Revision: 1.27 $
 * $Date: 2000/03/10 19:50:45 $
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Connections to Prelude entities:
 * Texts, Names, Instances, Classes, Types, Kinds and Modules
 * ------------------------------------------------------------------------*/

extern Text  textPrelude;
extern Text  textNum;                   /* used to process default decls   */
extern Text  textCcall;                 /* used to process foreign import  */
extern Text  textStdcall;               /*         ... and foreign export  */
extern Text  textPlus;                  /* Used to recognise n+k patterns  */


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
extern Name  nameReturn,  nameBind;     /* for translating monad comps     */
extern Name  nameMFail;
extern Name  nameListMonad;             /* builder function for List Monad */
extern Name  namePrint;                 /* printing primitive              */
extern Name  nameCreateAdjThunk;        /* f-x-dyn: create adjustor thunk  */
extern Name  nameShow;
extern Name  namePutStr;
extern Name  nameRunIO_toplevel;

/* The following data constructors are used to make boxed but 
 * unpointed values pointed and require no special treatment
 * by the code generator. */
extern Name nameMkInteger;
extern Name nameMkPrimArray;            
extern Name nameMkPrimByteArray;
extern Name nameMkRef;                  
extern Name nameMkPrimMutableArray;     
extern Name nameMkPrimMutableByteArray; 
extern Name nameMkThreadId;  
extern Name nameMkPrimMVar;  
#ifdef PROVIDE_FOREIGN
extern Name nameMkForeign;   
#endif
#ifdef PROVIDE_WEAK
extern Name nameMkWeak;
#endif

/* The following data constructors are used to box unboxed
 * arguments and are treated differently by the code generator.
 * That is, they have primop `elem` {INT_REP,FLOAT_REP,...}. */
#define boxingConRep(con) ((AsmRep)(name(con).primop))
#define isBoxingCon(con) (isName(con) && boxingConRep(con) != 0)
extern Name nameMkC;
extern Name nameMkI;
extern Name nameMkW;
extern Name nameMkA;
extern Name nameMkF;
extern Name nameMkD;
extern Name nameMkStable;    

/* used while desugaring */
extern Name nameId;
extern Name nameOtherwise;
extern Name nameUndefined;              /* generic undefined value         */

/* used in pattern match */
extern Name namePmSub;
extern Name nameSel;

/* used in translation */
extern Name nameEq;     
extern Name namePMFail;
extern Name nameEqChar;
extern Name nameEqInteger;
extern Name namePmInt;
extern Name namePmInteger;
extern Name namePmDouble;
extern Name namePmLe;
extern Name namePmSubtract;
extern Name namePmFromInteger;
extern Name nameMkIO;
extern Name nameUnpackString;
extern Name namePrimSeq;
extern Name nameMap;
extern Name nameMinus;


extern Class classMonad;                /* Monads                          */
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


extern Type typeProgIO;                 /* For the IO monad, IO a	   */
extern Type typeArrow;                  /* Builtin type constructors       */
extern Type typeList;
extern Type typeUnit;
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
extern Type typeOrdering;
extern List  stdDefaults;               /* List of standard default types  */

/* For every primitive type provided by the runtime system,
 * we construct a Haskell type using a declaration of the form:
 *
 *   data Int  -- no constructors given
 */
extern Type typeChar;
extern Type typeInt;
extern Type typeInteger;
extern Type typeWord;
extern Type typeAddr;
extern Type typePrimArray;            
extern Type typePrimByteArray;
extern Type typeRef;                  
extern Type typePrimMutableArray;     
extern Type typePrimMutableByteArray; 
extern Type typeFloat;
extern Type typeDouble;
extern Type typeStable;
extern Type typeThreadId;
extern Type typeMVar;
#ifdef PROVIDE_WEAK
extern Type typeWeak;
#endif
#ifdef PROVIDE_FOREIGN
extern Type typeForeign;
#endif

/* And a smaller number of types defined in plain Haskell */
extern Type typeList;
extern Type typeUnit;
extern Type typeString;
extern Type typeBool;
extern Type typeST;
extern Type typeIO;
extern Type typeException;



extern Module modulePrelude;

extern  Kind  starToStar;                /* Type -> Type                    */






#if TREX
extern Name  nameRecExt;                /* Extend a record                 */
extern Name  nameRecBrk;                /* Break a record                  */
extern Name  nameAddEv;                 /* Addition of evidence values     */
extern Name  nameRecSel;                /* Select a record                 */
extern Name  nameRecShw;                /* Show a record                   */
extern Name  nameShowRecRow;            /* Used to output rows             */
extern Name  nameRecEq;                 /* Compare records                 */
extern Name  nameEqRecRow;              /* Used to compare rows            */
extern Name  nameInsFld;                /* Field insertion routine         */
extern Name  nameNoRec;                 /* The empty record                */
extern Type  typeNoRow;                 /* The empty row                   */
extern Type  typeRec;                   /* Record formation                */
extern Kind  extKind;                   /* Kind of extension, *->row->row  */
#endif


/* --------------------------------------------------------------------------
 * Constructions from the above names, types, etc.
 * ------------------------------------------------------------------------*/


extern Type  arrow;                     /* mkOffset(0) -> mkOffset(1)      */
extern Type  listof;                    /* [ mkOffset(0) ]                 */
extern Cell  predNum;                   /* Num (mkOffset(0))               */
extern Cell  predFractional;            /* Fractional (mkOffset(0))        */
extern Cell  predIntegral;              /* Integral (mkOffset(0))          */
extern Cell  predMonad;                 /* Monad (mkOffset(0))             */


extern Type  arrow;                     /* mkOffset(0) -> mkOffset(1)      */
extern       Type  boundPair;;                 /* (mkOffset(0),mkOffset(0))       */
extern       Type  listof;;                    /* [ mkOffset(0) ]                 */
extern       Type  typeVarToVar;;              /* mkOffset(0) -> mkOffset(0)      */

extern       Cell  predNum;;                   /* Num (mkOffset(0))               */
extern       Cell  predFractional;;            /* Fractional (mkOffset(0))        */
extern       Cell  predIntegral;;              /* Integral (mkOffset(0))          */
extern       Kind  starToStar;;                /* Type -> Type                    */
extern       Cell  predMonad;;                 /* Monad (mkOffset(0))             */

#define fn(from,to)  ap(ap(typeArrow,from),to)  /* make type: from -> to   */

#define aVar            mkOffset(0)     /* Simple skeleton for type var    */
extern Type boundPair;                  /* (mkOffset(0),mkOffset(0))       */

#define consChar(c) ap(nameCons,mkChar(c))

/* --------------------------------------------------------------------------
 * Umm ....
 * ------------------------------------------------------------------------*/

extern Bool  haskell98;                 /* TRUE => Haskell 98 compatibility*/
extern Bool  combined;                  /* TRUE => combined operation      */
extern Bool  debugSC;			/* TRUE => print SC to screen  */
extern Bool  kindExpert;                /* TRUE => display kind errors in  */
                                        /*         full detail             */
extern Bool  allowOverlap;              /* TRUE => allow overlapping insts */

extern String repeatStr;                /* Repeat last command string      */
extern String hugsEdit;                 /* String for editor command       */
extern String hugsPath;                 /* String for file search path     */
extern String projectPath;              /* String for project search path  */

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
extern Bool  showInstRes;               /*TRUE => show instance resolution */

extern Int   cutoff;                    /* Constraint Cutoff depth         */

extern List diVars;                     /* deriving: cache of names        */
extern Int  diNum;                      /* also for deriving               */
extern List cfunSfuns;                  /* List of (Cfun,[SelectorVar])    */

#if USE_PREPROCESSOR
extern String preprocessor;             /* preprocessor command            */
#endif


/* --------------------------------------------------------------------------
 * Function prototypes etc...
 * ------------------------------------------------------------------------*/



#define RESET    1            /* reset subsystem                           */
#define MARK     2            /* mark parts of graph in use by subsystem   */
#define PREPREL  3            /* do startup actions before Prelude loading */
#define POSTPREL 4            /* do startup actions after Prelude loading  */
#define EXIT     5            /* Take action immediately before exit()     */
#define BREAK    6            /* Take action after program break           */
#define GCDONE   7            /* Restore subsystem invariants after GC     */

/* PREPREL was formerly called INSTALL.  POSTPREL doesn't have an analogy
   in the old Hugs. 
*/
extern Void everybody Args((Int));
extern  Void   linkControl      Args((Int));
extern  Void   deriveControl    Args((Int));
extern  Void   translateControl Args((Int));
extern  Void   codegen          Args((Int));
extern  Void   machdep          Args((Int));
extern Void liftControl ( Int what );
extern  Void   substitution     Args((Int));
extern  Void   typeChecker      Args((Int));
extern Void  interface        Args((Int));



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
extern  Name   newDSel             Args((Class,Int));
#if TREX
extern  Inst   addRecShowInst   Args((Class,Ext));
extern  Inst   addRecEqInst     Args((Class,Ext));
#endif
extern List offsetTyvarsIn          Args((Type,List));


extern  List   typeVarsIn	Args((Cell,List,List,List));
extern  List   oclose		Args((List,List));
extern  List   zonkTyvarsIn	Args((Type,List));
extern  Type   zonkTyvar	Args((Int));
extern  Type   zonkType		Args((Type,Int));
extern  Void   primDefn         Args((Cell,List,Cell));
extern  Void   defaultDefn      Args((Int,List));
extern  Void   checkExp         Args((Void));
extern  Type   conToTagType     Args((Tycon));
extern  Type   tagToConType     Args((Tycon));
extern Int          visitClass          Args((Class));

#if EXPLAIN_INSTANCE_RESOLUTION
extern  Void   checkContext	Args((Void));
#endif
extern  Void   checkDefns       Args((Void));
extern  Bool   h98Pred          Args((Bool,Cell));
extern  Cell   h98Context       Args((Bool,List));
extern  Void   h98CheckCtxt     Args((Int,String,Bool,List,Inst));
extern  Void   h98CheckType     Args((Int,String,Cell,Type));
extern  Void   h98DoesntSupport Args((Int,String));

extern Int     userArity           Args((Name));
extern List    deriveEq            Args((Tycon));
extern List    deriveOrd           Args((Tycon));
extern List    deriveEnum          Args((Tycon));
extern List    deriveIx            Args((Tycon));
extern List    deriveShow          Args((Tycon));
extern List    deriveRead          Args((Cell));
extern List    deriveBounded       Args((Tycon));
extern List    checkPrimDefn       Args((Triple));

extern  Void  foreignImport    Args((Cell,Text,Pair,Cell,Cell));
extern  List  foreignImports;            /* foreign import declarations     */
extern  Void  implementForeignImport Args((Name));
extern  Void  foreignExport   Args((Cell,Text,Cell,Cell,Cell));
extern  List  foreignExports;            /* foreign export declarations     */
extern  Void  implementForeignExport Args((Name));

extern Type primType( Int /*AsmMonad*/ monad, String a_kinds, String r_kinds );

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
extern  Void   linkPrimitiveNames(void);

extern  Void   compiler         Args((Int));
extern  Void   compileDefns     Args((Void));
extern  Void   compileExp       Args((Void));
extern  Bool   failFree         Args((Cell));
extern  Int    discrArity       Args((Cell));

extern  Addr   codeGen          Args((Name,Int,Cell));
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


/*---------------------------------------------------------------------------
 * Debugging printers, and output-ery
 *-------------------------------------------------------------------------*/

extern Void ppScripts ( Void );
extern Void ppModules ( Void );

extern Void printStg( FILE *fp, Cell /*StgVar*/ b);
            
extern Void ppStg        ( Cell /*StgVar*/ v );
extern Void ppStgExpr    ( Cell /*StgExpr*/ e );
extern Void ppStgRhs     ( Cell /*StgRhs*/ rhs );
extern Void ppStgAlts    ( List alts );
extern Void ppStgPrimAlts( List alts );
extern Void ppStgVars    ( List vs );

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


/*---------------------------------------------------------------------------
 * Crude profiling (probably doesn't work)
 *-------------------------------------------------------------------------*/

#ifdef CRUDE_PROFILING
extern void cp_init ( void );
extern void cp_enter ( Cell /*StgVar*/ );
extern void cp_bill_words ( int );
extern void cp_bill_insns ( int );
extern void cp_show ( void );
#endif


/*---------------------------------------------------------------------------
 * For dynamic.c and general object-related stuff
 *-------------------------------------------------------------------------*/

extern void*      getDLLSymbol   Args((Int,String,String));
extern Bool       stdcallAllowed Args((void));

#if LEADING_UNDERSCORE
#define MAYBE_LEADING_UNDERSCORE(sss)     _##sss
#define MAYBE_LEADING_UNDERSCORE_STR(sss) "_" sss
#else
#define MAYBE_LEADING_UNDERSCORE(sss)     sss
#define MAYBE_LEADING_UNDERSCORE_STR(sss) sss
#endif


/*---------------------------------------------------------------------------
 * Interrupting execution (signals, allowBreak):
 *-------------------------------------------------------------------------*/

extern Bool breakOn      Args((Bool));
extern Bool broken;                     /* indicates interrupt received    */

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


/*---------------------------------------------------------------------------
 * To do with reading interface and object files
 *-------------------------------------------------------------------------*/

extern Cell parseInterface        Args((String,Long));
extern ZPair readInterface      Args((String,Long));
extern Bool  processInterfaces  Args((Void));


extern Void getFileSize       Args((String, Long *));

extern void  ifLinkConstrItbl ( Name n );


extern List /* of ZTriple(I_INTERFACE, 
                          Text--name of obj file, 
                          Int--size of obj file) */
             ifaces_outstanding;


extern Void hi_o_namesFromSrcName Args((String,String*,String* oName));

extern String getExtraObjectInfo ( String primaryObjectName,
                                   String extraFileName,
                                   Int*   extraFileSize );


/* --------------------------------------------------------------------------
 * Interpreter command structure
 * ------------------------------------------------------------------------*/

typedef Int Command;

struct cmd {
    String cmdString;
    Command cmdCode;
};

extern Command readCommand Args((struct cmd *, Char, Char));

#define EDIT    0
#define FIND    1
#define LOAD    2
#define ALSO    3
#define PROJECT 4
#define RELOAD  5
#define EVAL    6
#define TYPEOF  7
#define HELP    8
#define NAMES   9
#define BADCMD  10
#define SET     11
#define QUIT    12
#define SYSTEM  13
#define CHGDIR  14
#define INFO    15
#define COLLECT 16
#define SETMODULE 17
#define DUMP    18
#define STATS   19
#define BROWSE  20
#define XPLAIN  21
#define PNTVER  22
#define NOCMD   23


/* --------------------------------------------------------------------------
 * STG Syntax:
 * 
 *   Rhs     -> STGCON   (Con, [Atom])
 *            | STGAPP   (Var, [Atom])     -- delayed application
 *            | Expr                       
 *                                         
 *   Expr    -> LETREC   ([Var],Expr)      -- Vars contain their bound value
 *            | LAMBDA   ([Var],Expr)      -- all vars bound to NIL
 *            | CASE     (Expr,[Alt])      -- algebraic case
 *            | PRIMCASE (Expr,[PrimAlt])  -- primitive case
 *            | STGPRIM  (Prim,[Atom])     
 *            | STGAPP   (Var, [Atom])     -- tail call
 *            | Var                        -- Abbreviation for STGAPP(Var,[])
 *                                         
 *   Atom    -> Var                        
 *            | CHAR                       -- unboxed
 *            | INT                        -- unboxed
 *            | BIGNUM                     -- unboxed
 *            | FLOAT                      -- unboxed
 *            | ADDR                       -- unboxed
 *            | STRING                     -- boxed
 *                                         
 *   Var     -> STGVAR   (Rhs,StgRep,info) -- let, case or lambda bound
 *            | Name                       -- let-bound (effectively)
 *                                         -- always unboxed (PTR_REP)
 *
 *   Alt     -> DEEFALT (Var,Expr)         -- var bound to NIL
 *            | CASEALT (Con,[Var],Expr)   -- vars bound to NIL; 
 *                                         -- Con is Name or TUPLE
 *   PrimAlt -> PRIMALT ([Var],Expr)       -- vars bound to NIL or int
 * 
 * We use pointer equality to distinguish variables.
 * The info field of a Var is used as follows in various phases:
 * 
 * Translation:      unused (set to NIL on output)
 * Freevar analysis: list of free vars after
 * Lambda lifting:   freevar list or UNIT on input, discarded after
 * Code generation:  unused
 * Optimisation:     number of uses (sort-of) of let-bound variable
 * ------------------------------------------------------------------------*/

typedef Cell   StgRhs;
typedef Cell   StgExpr;
typedef Cell   StgAtom;
typedef Cell   StgVar;       /* Could be a Name or an STGVAR */
typedef Cell   StgCaseAlt;
typedef Cell   StgPrimAlt;
typedef Cell   StgDiscr;
typedef Cell   StgRep;  /* PTR_REP | .. DOUBLE_REP */

#define mkStgLet(binds,body)       ap(LETREC,pair(binds,body))
#define stgLetBinds(e)             fst(snd(e))
#define stgLetBody(e)              snd(snd(e))

#define mkStgPrimVar(rhs,rep,info) ap(STGVAR,triple(rhs,rep,info))
#define stgVarBody(e)              fst3(snd(e))
#define stgVarRep(e)               snd3(snd(e))
#define stgVarInfo(e)              thd3(snd(e))

#define mkStgCase(scrut,alts)      ap(CASE,pair(scrut,alts))
#define stgCaseScrut(e)            fst(snd(e))
#define stgCaseAlts(e)             snd(snd(e))

#define mkStgCaseAlt(con,vs,e)     ap(CASEALT,triple(con,vs,e))
#define stgCaseAltCon(alt)         fst3(snd(alt))
#define stgCaseAltVars(alt)        snd3(snd(alt))
#define stgCaseAltBody(alt)        thd3(snd(alt))

#define mkStgDefault(v,e)          ap(DEEFALT,pair(v,e))
#define stgDefaultVar(alt)         fst(snd(alt))
#define stgDefaultBody(alt)        snd(snd(alt))
#define isDefaultAlt(alt)          (fst(alt)==DEEFALT)

#define mkStgPrimCase(scrut,alts)  ap(PRIMCASE,pair(scrut,alts))
#define stgPrimCaseScrut(e)        fst(snd(e))
#define stgPrimCaseAlts(e)         snd(snd(e))

#define mkStgPrimAlt(vs,body)      ap(PRIMALT,pair(vs,body))
#define stgPrimAltVars(alt)        fst(snd(alt))
#define stgPrimAltBody(alt)        snd(snd(alt))

#define mkStgApp(fun,args)         ap(STGAPP,pair(fun,args))
#define stgAppFun(e)               fst(snd(e))
#define stgAppArgs(e)              snd(snd(e))

#define mkStgPrim(op,args)         ap(STGPRIM,pair(op,args))
#define stgPrimOp(e)               fst(snd(e))
#define stgPrimArgs(e)             snd(snd(e))

#define mkStgCon(con,args)         ap(STGCON,pair(con,args))
#define stgConCon(e)               fst(snd(e))
#define stgConArgs(e)              snd(snd(e))

#define mkStgLambda(args,body)     ap(LAMBDA,pair(args,body))
#define stgLambdaArgs(e)           fst(snd(e))
#define stgLambdaBody(e)           snd(snd(e))


/* --------------------------------------------------------------------------
 * Utility functions for manipulating STG syntax trees.
 * ------------------------------------------------------------------------*/

extern int stgConTag  ( StgDiscr d );
extern void* stgConInfo ( StgDiscr d );
extern int stgDiscrTag( StgDiscr d );

extern List    makeArgs      ( Int );
extern StgExpr makeStgLambda ( List args,  StgExpr body );
extern StgExpr makeStgApp    ( StgVar fun, List args );
extern StgExpr makeStgLet    ( List binds, StgExpr body );
extern StgExpr makeStgIf     ( StgExpr cond, StgExpr e1, StgExpr e2 );
extern Bool    isStgVar      ( StgRhs rhs );
extern Bool    isAtomic      ( StgRhs rhs );
extern StgVar  mkStgVar      ( StgRhs rhs, Cell info );

#define mkStgRep(c) mkChar(c)


/* --------------------------------------------------------------------------
 * STG/backendish functions
 * ------------------------------------------------------------------------*/

extern Void stgDefn       Args(( Name n, Int arity, Cell e ));

extern  Void   implementForeignImport Args((Name));
extern  Void   implementForeignExport Args((Name));
extern  Void   implementCfun          Args((Name, List));
extern  Void   implementConToTag Args((Tycon));
extern  Void   implementTagToCon Args((Tycon));
extern  Void   implementPrim     Args((Name));
extern  Void   implementTuple    Args((Int));
#if TREX                         
extern  Name   implementRecShw   Args((Text));
extern  Name   implementRecEq    Args((Text));
#endif

/* Association list storing globals assigned to dictionaries, tuples, etc */
extern List stgGlobals;

extern List liftBinds( List binds );
extern StgExpr substExpr ( List sub, StgExpr e );
extern List freeVarsBind Args((List, StgVar));


extern Void  cgBinds       Args((StgRhs));
extern void* closureOfVar  Args((StgVar));
extern char* lookupHugsName Args((void*));


/* --------------------------------------------------------------------------
 * Definitions for substitution data structure and operations.
 * ------------------------------------------------------------------------*/

typedef struct {                        /* Each type variable contains:    */
    Type bound;                         /* A type skeleton (unbound==NIL)  */
    Int  offs;                          /* Offset for skeleton             */
    Kind kind;                          /* kind annotation                 */
} Tyvar;

#if     FIXED_SUBST                     /* storage for type variables      */
extern  Tyvar           tyvars[];
#else
extern  Tyvar           *tyvars;        /* storage for type variables      */
#endif
extern  Int             typeOff;        /* offset of result type           */
extern  Type            typeIs;         /* skeleton of result type         */
extern  Int             typeFree;       /* freedom in instantiated type    */
extern  List            predsAre;       /* list of predicates in type      */
extern  List            genericVars;    /* list of generic vars            */
extern  List            btyvars;        /* explicitly scoped type vars     */

#define tyvar(n)        (tyvars+(n))    /* nth type variable               */
#define tyvNum(t)       ((t)-tyvars)    /* and the corresp. inverse funct. */
#define isBound(t)      (((t)->bound) && ((t)->bound!=SKOLEM))
#define aVar            mkOffset(0)     /* Simple skeletons for type vars  */
#define bVar            mkOffset(1)
#define enterBtyvs()    btyvars = cons(NIL,btyvars)
#define leaveBtyvs()    btyvars = tl(btyvars)

#define deRef(tyv,t,o)  while ((tyv=getTypeVar(t,o)) && isBound(tyv)) { \
                            t = tyv->bound;                             \
                            o = tyv->offs;                              \
                        }

                                        /* offs values when isNull(bound): */
#define FIXED_TYVAR     0               /* fixed in current assumption     */
#define UNUSED_GENERIC  1               /* not fixed, not yet encountered  */
#define GENERIC         2               /* GENERIC+n==nth generic var found*/

extern  char            *unifyFails;    /* Unification error message       */

extern Void  emptySubstitution  Args((Void));
extern Int   newTyvars          Args((Int));
#define      newKindvars(n)     newTyvars(n)
extern Int   newKindedVars      Args((Kind));
extern Kind  simpleKind         Args((Int));
extern Void  instantiate        Args((Type));

extern Pair  findBtyvs          Args((Text));
extern Void  markBtyvs          Args((Void));
extern Type  localizeBtyvs      Args((Type));

extern Tyvar *getTypeVar        Args((Type,Int));
extern Void  tyvarType          Args((Int));
extern Void  bindTv             Args((Int,Type,Int));
extern Cell  getDerefHead       Args((Type,Int));
extern Void  expandSyn          Args((Tycon, Int, Type *, Int *));

extern Void  clearMarks         Args((Void));
extern Void  markAllVars        Args((Void));
extern Void  resetGenerics      Args((Void));
extern Void  markTyvar          Args((Int));
extern Void  markType           Args((Type,Int));
extern Void  markPred           Args((Cell));

extern Type  copyTyvar          Args((Int));
extern Type  copyType           Args((Type,Int));
extern Cell  copyPred           Args((Cell,Int));
extern Type  dropRank2          Args((Type,Int,Int));
extern Type  dropRank1          Args((Type,Int,Int));
extern Void  liftRank2Args      Args((List,Int,Int));
extern Type  liftRank2          Args((Type,Int,Int));
extern Type  liftRank1          Args((Type,Int,Int));
#ifdef DEBUG_TYPES
extern Type  debugTyvar         Args((Int));
extern Type  debugType          Args((Type,Int));
#endif
extern Kind  copyKindvar        Args((Int));
extern Kind  copyKind           Args((Kind,Int));

extern Bool  eqKind             Args((Kind,Kind));
extern Kind  getKind            Args((Cell,Int));

extern List  genvarTyvar        Args((Int,List));
extern List  genvarType         Args((Type,Int,List));

extern Bool  doesntOccurIn      Args((Tyvar*,Type,Int));
extern Bool  unify              Args((Type,Int,Type,Int));
extern Bool  kunify             Args((Kind,Int,Kind,Int));

extern Void  typeTuple          Args((Cell));
extern Void  varKind            Args((Int));

extern Bool  samePred           Args((Cell,Int,Cell,Int));
extern Bool  matchPred          Args((Cell,Int,Cell,Int));
extern Bool  unifyPred          Args((Cell,Int,Cell,Int));
extern Inst  findInstFor        Args((Cell,Int));

extern Void  improve		Args((Int,List,List));
extern Void  improve1		Args((Int,List,Cell,Int));

extern Bool  sameSchemes	Args((Type,Type));
extern Bool  sameType		Args((Type,Int,Type,Int));
extern Bool  matchType		Args((Type,Int,Type,Int));
extern Bool  typeMatches        Args((Type,Type));

/*-------------------------------------------------------------------------*/
