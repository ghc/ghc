
/* --------------------------------------------------------------------------
 * Defines storage datatypes: Text, Name, Module, Tycon, Cell, List, Pair,
 * Triple, ...
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: storage.h,v $
 * $Revision: 1.37 $
 * $Date: 2000/03/28 10:20:56 $
 * ------------------------------------------------------------------------*/

#define DEBUG_STORAGE

/* --------------------------------------------------------------------------
 * Typedefs for main data types:
 * Many of these type names are used to indicate the intended us of a data
 * item, rather than for type checking purposes.  Sadly (although sometimes,
 * fortunately), the C compiler cannot distinguish between the use of two
 * different names defined to be synonyms for the same types.
 * ------------------------------------------------------------------------*/

typedef Int          Text;                       /* text string            */
typedef Unsigned     Syntax;                     /* syntax (assoc,preced)  */
typedef Int          Cell;                       /* general cell value     */
typedef Cell far     *Heap;                      /* storage of heap        */
typedef Cell         Pair;                       /* pair cell              */
typedef Int          StackPtr;                   /* stack pointer          */
typedef Cell         Offset;                     /* offset/generic variable*/
typedef Int          Module;                     /* module                 */
typedef Cell         Tycon;                      /* type constructor       */
typedef Cell         Type;                       /* type expression        */
typedef Cell         Kind;                       /* kind expression        */
typedef Cell         Kinds;                      /* list of kinds          */
typedef Cell         Constr;                     /* constructor expression */
typedef Cell         Name;                       /* named value            */
typedef Cell         Class;                      /* type class             */
typedef Cell         Inst;                       /* instance of type class */
typedef Cell         Triple;                     /* triple of cell values  */
typedef Cell         List;                       /* list of cells          */
typedef Cell         Bignum;                     /* bignum integer         */
typedef Cell         Float;                      /* floating pt literal    */
#if TREX
typedef Cell         Ext;                        /* extension label        */
#endif

typedef Cell         ConId;
typedef Cell         VarId;
typedef Cell         QualId;
typedef Cell         ConVarId;

/* --------------------------------------------------------------------------
 * Address ranges.
 * 
 * -heapSize .. -1                                    cells in the heap
 * 0                                                  NIL
 *
 * TAG_NONPTR_MIN(100) .. TAG_NONPTR_MAX(115)         non pointer tags
 * TAG_PTR_MIN(200)    .. TAG_PTR_MAX(298)            pointer tags
 * TAG_SPEC_MIN(400)   .. TAG_SPEC_MAX(425)           special tags
 * OFF_MIN(1,000)      .. OFF_MAX(1,999)              offsets
 * CHARR_MIN(3,000)    .. CHARR_MAX(3,255)            chars
 *
 * SMALL_INT_MIN(100,000) .. SMALL_INT_MAX(499,999)   smallish ints
 *              (300,000 denotes 0)
 *
 * NAME_BASE_ADDR   (1,000,000 .. 1,899,999)          names
 * TYCON_BASE_ADDR  (2,000,000 .. 2,899,999)          tycons
 * CCLASS_BASE_ADDR (3,000,000 .. 3,899,999)          classes
 * INST_BASE_ADDR   (4,000,000 .. 4,899,999)          instances
 * MODULE_BASE_ADDR (5,000,000 .. 5,899,999)          modules
 * INVAR_BASE_ADDR  (6,000,000 .. 6,899,999)          invented var names
 * INDVAR_BASE_ADDR (7,000,000 .. 7,899,999)          invented dict var names
 * TEXT_BASE_ADDR   (8,000,000 .. 8M +TEXT_SIZE-1)    text
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Text storage:
 * provides storage for the characters making up identifier and symbol
 * names, string literals, character constants etc...
 * ------------------------------------------------------------------------*/

extern  String       textToStr            ( Text );
extern  Text         findText             ( String );
extern  Text         inventText           ( Void );
extern  Text         inventDictText       ( Void );
extern  Bool         inventedText         ( Text );
extern  Text         enZcodeThenFindText  ( String );
extern  Text         unZcodeThenFindText  ( String );

/* Variants of textToStr and syntaxOf which work for idents, ops whether
 * qualified or unqualified.
 */
extern  String       identToStr         ( Cell );
extern	Text	     fixLitText	 	( Text );
extern  Syntax       identSyntax        ( Cell );
extern  Syntax       defaultSyntax      ( Text );

#define INVAR_BASE_ADDR  6000000
#define INVAR_MAX_AVAIL  900000
#define isInventedVar(c) (INVAR_BASE_ADDR<=(c) \
                          && (c)<INVAR_BASE_ADDR+INVAR_MAX_AVAIL)

#define INDVAR_BASE_ADDR 7000000
#define INDVAR_MAX_AVAIL 900000
#define isInventedDictVar(c) (INDVAR_BASE_ADDR<=(c) \
                              && (c)<INDVAR_BASE_ADDR+INDVAR_MAX_AVAIL)

#define TEXT_BASE_ADDR   8000000
#define isText(c) (TEXT_BASE_ADDR<=(c) \
                  && (c)<TEXT_BASE_ADDR+TEXT_SIZE)

/* --------------------------------------------------------------------------
 * Specification of syntax (i.e. default written form of application)
 * ------------------------------------------------------------------------*/

#define MIN_PREC  0                    /* weakest binding operator         */
#define MAX_PREC  9                    /* strongest binding operator       */
#define FUN_PREC  (MAX_PREC+2)         /* binding of function symbols      */
#define DEF_PREC  MAX_PREC
#define APPLIC    0                    /* written applicatively            */
#define LEFT_ASS  1                    /* left associative infix           */
#define RIGHT_ASS 2                    /* right associative infix          */
#define NON_ASS   3                    /* non associative infix            */
#define DEF_ASS   LEFT_ASS

#define UMINUS_PREC  6                  /* Change these settings at your   */
#define UMINUS_ASSOC LEFT_ASS           /* own risk; they may not work!    */

#define assocOf(x)      ((x)&NON_ASS)
#define precOf(x)       ((x)>>2)
#define mkSyntax(a,p)   ((a)|((p)<<2))
#define DEF_OPSYNTAX    mkSyntax(DEF_ASS,DEF_PREC)
#define NO_SYNTAX       (-1)

extern  Void   addSyntax  ( Int,Text,Syntax );
extern  Syntax syntaxOf   ( Text );

/* --------------------------------------------------------------------------
 * Heap storage:
 * Provides a garbage collectable heap for storage of expressions etc.
 * ------------------------------------------------------------------------*/

#define heapAlloc(s) (Heap)(farCalloc(s,sizeof(Cell)))
extern  Int          heapSize;
extern  Heap         heapFst, heapSnd;
extern  Heap         heapTopFst;
extern  Heap         heapTopSnd;
extern  Bool         consGC;            /* Set to FALSE to turn off gc from*/
                                        /* C stack; use with extreme care! */
extern  Int          cellsRecovered;    /* cells recovered by last gc      */

#define fst(c)       heapTopFst[c]
#define snd(c)       heapTopSnd[c]

extern  Pair         pair            ( Cell,Cell );
extern  Void         garbageCollect  ( Void );

extern  Void         overwrite       ( Pair,Pair );
extern  Cell         markExpr        ( Cell );
extern  Void         markWithoutMove ( Cell );

#define mark(v)      v=markExpr(v)

#define isPair(c)    ((c)<0)
#define isGenPair(c) ((c)<0 && -heapSize<=(c))

extern  Cell         whatIs    ( Cell );

/* --------------------------------------------------------------------------
 * Pairs in the heap fall into three categories.
 *
 * pair(TAG_NONPTR,y)
 *    used to denote that the second element of the pair is to be treated
 *    in some special way (eg is a integer or Text), and specifically is not
 *    a heap pointer
 * 
 * pair(TAG_PTR,y)
 *    to indicate that the second element of the pair is a normal 
 *    heap pointer, which should be followed at GC time
 * 
 * pair(x,y)
 *    is a genuine pair, where both components are heap pointers.
 * ------------------------------------------------------------------------*/

#if !defined(SIZEOF_VOID_P) || !defined(SIZEOF_INT)
#error SIZEOF_VOID_P or SIZEOF_INT is not defined
#endif

#define isTagNonPtr(c) (TAG_NONPTR_MIN<=(c) && (c)<=TAG_NONPTR_MAX)
#define isTagPtr(c)    (TAG_PTR_MIN<=(c) && (c)<=TAG_PTR_MAX)
#define isTag(c)       (isTagNonPtr(c) || isTagPtr(c))

/* --------------------------------------------------------------------------
 * Tags for non-pointer cells.
 * ------------------------------------------------------------------------*/

#define TAG_NONPTR_MIN 100
#define TAG_NONPTR_MAX 115

#define FREECELL     100          /* Free list cell:          snd :: Cell  */
#define VARIDCELL    101          /* Identifier variable:     snd :: Text  */
#define VAROPCELL    102          /* Operator variable:       snd :: Text  */
#define DICTVAR      103          /* Dictionary variable:     snd :: Text  */
#define CONIDCELL    104          /* Identifier constructor:  snd :: Text  */
#define CONOPCELL    105          /* Operator constructor:    snd :: Text  */
#define STRCELL      106          /* String literal:          snd :: Text  */
#define INTCELL      107          /* Int literal:             snd :: Int   */
#define ADDPAT       108          /* (_+k) pattern discr:     snd :: Int   */
#define FLOATCELL    109          /* Floating Pt literal:     snd :: Text  */
#define BIGCELL      110          /* Integer literal:         snd :: Text  */
#define PTRCELL      111          /* C Heap Pointer           snd :: Ptr   */
#define CPTRCELL     112          /* Native code pointer      snd :: Ptr   */

#if IPARAM
#define IPCELL       113       	  /* Imp Param Cell:	      snd :: Text  */
#define IPVAR	     114	  /* ?x:		      snd :: Text  */
#endif

#if TREX
#define EXTCOPY      115          /* Copy of an Ext:          snd :: Text  */
#endif

#define qmodOf(c)       (textOf(fst(snd(c))))    /* c ::  QUALIDENT        */
#define qtextOf(c)      (textOf(snd(snd(c))))    /* c ::  QUALIDENT        */
#define mkVar(t)        ap(VARIDCELL,t)
#define mkVarop(t)      ap(VAROPCELL,t)
#define mkCon(t)        ap(CONIDCELL,t)
#define mkConop(t)      ap(CONOPCELL,t)
#define mkQVar(m,t)     ap(QUALIDENT,pair(mkCon(m),mkVar(t)))
#define mkQCon(m,t)     ap(QUALIDENT,pair(mkCon(m),mkCon(t)))
#define mkQVarOp(m,t)   ap(QUALIDENT,pair(mkCon(m),mkVarop(t)))
#define mkQConOp(m,t)   ap(QUALIDENT,pair(mkCon(m),mkConop(t)))
#define mkQualId(m,t)   ap(QUALIDENT,pair(m,t))
#define intValOf(c)     (snd(c))
#define inventVar()     mkVar(inventText())
#define mkDictVar(t)    ap(DICTVAR,t)
#define inventDictVar() mkDictVar(inventDictText())
#define mkStr(t)        ap(STRCELL,t)
#if IPARAM
#define mkIParam(c)	ap(IPCELL,snd(c))
#define isIP(p)		(whatIs(p) == IPCELL)
#define ipMatch(pi, t)	(isIP(fun(pi)) && textOf(fun(pi)) == t)
#define ipVar(pi)	textOf(fun(pi))
#else
#define isIP(p)		FALSE
#endif

extern  Bool            isVar        ( Cell );
extern  Bool            isCon        ( Cell );
extern  Bool            isQVar       ( Cell );
extern  Bool            isQCon       ( Cell );
extern  Bool            isQualIdent  ( Cell );
extern  Bool            eqQualIdent  ( QualId c1, QualId c2 );
extern  Bool            isIdent      ( Cell );
extern  String          stringNegate ( String );
extern  Text            textOf       ( Cell );

#define isFloat(c)       (isPair(c) && fst(c)==FLOATCELL)
#define stringToFloat(s) pair(FLOATCELL,findText(s))
#define floatToString(f) textToStr(snd(f))
#define floatOf(f)       atof(floatToString(f))
#define mkFloat(f)       (f)  /* ToDo: is this right? */
#define floatNegate(f)   stringToFloat(stringNegate(floatToString(f)))

#define stringToBignum(s) pair(BIGCELL,findText(s))
#define bignumToString(b) textToStr(snd(b))

#define isPtr(c)        (isPair(c) && fst(c)==PTRCELL)
extern  Cell            mkPtr           ( Ptr );
extern  Ptr             ptrOf           ( Cell );
#define isCPtr(c)       (isPair(c) && fst(c)==CPTRCELL)
extern  Cell            mkCPtr          ( Ptr );
extern  Ptr             cptrOf          ( Cell );

/* --------------------------------------------------------------------------
 * Tags for pointer cells.
 * ------------------------------------------------------------------------*/

#define TAG_PTR_MIN 200
#define TAG_PTR_MAX 298

#define LETREC       200          /* LETREC     snd :: ([Decl],Exp)        */
#define COND         201          /* COND       snd :: (Exp,Exp,Exp)       */
#define LAMBDA       202          /* LAMBDA     snd :: Alt                 */
#define FINLIST      203          /* FINLIST    snd :: [Exp]               */
#define DOCOMP       204          /* DOCOMP     snd :: (Exp,[Qual])        */
#define BANG         205          /* BANG       snd :: Type                */
#define COMP         206          /* COMP       snd :: (Exp,[Qual])        */
#define ASPAT        207          /* ASPAT      snd :: (Var,Exp)           */
#define ESIGN        208          /* ESIGN      snd :: (Exp,Type)          */
#define RSIGN        209          /* RSIGN      snd :: (Rhs,Type)          */
#define CASE         210          /* CASE       snd :: (Exp,[Alt])         */
#define NUMCASE      211          /* NUMCASE    snd :: (Exp,Disc,Rhs)      */
#define FATBAR       212          /* FATBAR     snd :: (Exp,Exp)           */
#define LAZYPAT      213          /* LAZYPAT    snd :: Exp                 */
#define DERIVE       214          /* DERIVE     snd :: Cell                */
#define BOOLQUAL     215          /* BOOLQUAL   snd :: Exp                 */
#define QWHERE       216          /* QWHERE     snd :: [Decl]              */
#define FROMQUAL     217          /* FROMQUAL   snd :: (Exp,Exp)           */
#define DOQUAL       218          /* DOQUAL     snd :: Exp                 */
#define MONADCOMP    219          /* MONADCOMP  snd :: ((m,m0),(Exp,[Qual])*/
#define GUARDED      220          /* GUARDED    snd :: [guarded exprs]     */
#define ARRAY        221          /* Array      snd :: (Bounds,[Values])   */
#define MUTVAR       222          /* Mutvar     snd :: Cell                */
#define HUGSOBJECT   223          /* HUGSOBJECT snd :: Cell                */

#if IPARAM
#define WITHEXP      224	  /* WITHEXP    snd :: [(Var,Exp)]	   */
#endif

#define POLYTYPE     225          /* POLYTYPE   snd :: (Kind,Type)         */
#define QUAL         226          /* QUAL       snd :: ([Classes],Type)    */
#define RANK2        227          /* RANK2      snd :: (Int,Type)          */
#define EXIST        228          /* EXIST      snd :: (Int,Type)          */
#define POLYREC      229          /* POLYREC    snd :: (Int,Type)          */
#define BIGLAM       230          /* BIGLAM     snd :: (vars,patterns)     */
#define CDICTS       231          /* CDICTS     snd :: ([Pred],Type)       */

#define LABC         232          /* LABC       snd :: (con,[(Vars,Type)]) */
#define CONFLDS      233          /* CONFLDS    snd :: (con,[Field])       */
#define UPDFLDS      234          /* UPDFLDS    snd :: (Exp,[con],[Field]) */
#if TREX
#define RECORD       235          /* RECORD     snd :: [Val]               */
#define EXTCASE      236          /* EXTCASE    snd :: (Exp,Disc,Rhs)      */
#define RECSEL       237          /* RECSEL     snd :: Ext                 */
#endif
#define IMPDEPS      238          /* IMPDEPS    snd :: [Binding]           */

#define QUALIDENT    239          /* Qualified identifier  snd :: (Id,Id)  */
#define HIDDEN       240          /* hiding import list    snd :: [Entity] */
#define MODULEENT    241          /* module in export list snd :: con      */

#define INFIX        242          /* INFIX      snd :: (see tidyInfix)     */
#define ONLY         243          /* ONLY       snd :: Exp                 */
#define NEG          244          /* NEG        snd :: Exp                 */

/* Used when parsing GHC interface files */
#define DICTAP       245          /* DICTAP     snd :: (QClassId,[Type])   */
#define UNBOXEDTUP   246          /* UNBOXEDTUP snd :: [Type]              */

#if SIZEOF_VOID_P != SIZEOF_INT
#define PTRCELL      247          /* C Heap Pointer snd :: (Int,Int)       */
#endif

/* STG syntax */
#define STGVAR       248          /* STGVAR     snd :: (StgRhs,info)       */
#define STGAPP       249          /* STGAPP     snd :: (StgVar,[Arg])      */
#define STGPRIM      250          /* STGPRIM    snd :: (PrimOp,[Arg])      */
#define STGCON       251          /* STGCON     snd :: (StgCon,[Arg])      */
#define PRIMCASE     252          /* PRIMCASE   snd :: (Expr,[PrimAlt])    */
#define DEEFALT      253          /* DEEFALT    snd :: (Var,Expr)          */
#define CASEALT      254          /* CASEALT    snd :: (Con,[Var],Expr)    */
#define PRIMALT      255          /* PRIMALT    snd :: ([Var],Expr)        */

/* Module groups */
#define GRP_REC      256          /* GRP_REC    snd :: [CONID]             */
#define GRP_NONREC   257          /* GRP_NONREC snd :: CONID               */


/* 
   Top-level interface entities 
   type Line             = Int  -- a line number 
   type ConVarId         = CONIDCELL | VARIDCELL
   type ExportListEntry  = ConVarId | (ConId, [ConVarId]) 
   type Associativity    = mkInt of LEFT_ASS | RIGHT_ASS | NON_ASS
   type Constr           = ((ConId, [((Type,VarId,Int))]))
               ((constr name, [((type, field name if any, strictness))]))
               strictness: 0 => none, 1 => !, 2 => !! (unpacked)
   All 2/3/4/5 tuples in the interface abstract syntax are done with
   z-tuples.
*/

#define I_INTERFACE  260  /* snd :: ((ConId, [I_IMPORT..I_VALUE])) 
                                    interface name, list of iface entities */

#define I_IMPORT     261  /* snd :: ((ConId, [ConVarId]))
                                    module name, list of entities          */

#define I_INSTIMPORT 262  /* snd :: NIL    -- not used at present          */

#define I_EXPORT     263  /* snd :: ((ConId, [ExportListEntry]))
                                    this module name?, entities to export  */

#define I_FIXDECL    264  /* snd :: ((NIL|Int, Associativity, ConVarId))   
                                    fixity, associativity, name            */

#define I_INSTANCE   265 /* snd :: ((Line, 
                                     [((VarId,Kind))], 
                                     Type, VarId, Inst))
                   lineno, 
                   forall-y bit (eg __forall [a b] =>),
                   other bit, eg { C a1 } -> { C2 a2 } -> ... -> { Cn an },
                   name of dictionary builder,
                   (after startGHCInstance) the instance table location    */

#define I_TYPE       266 /* snd :: ((Line, ConId, [((VarId,Kind))], Type))
                            lineno, tycon, kinded tyvars, the type expr    */

#define I_DATA       267 /* snd :: ((Line, [((QConId,VarId))], ConId, 
                                          [((VarId,Kind))], [Constr]) 
                            lineno, context, tycon, kinded tyvars, constrs 
                           An empty constr list means exported abstractly. */

#define I_NEWTYPE    268 /* snd :: ((Line, [((QConId,VarId))], ConId,
                                    [((VarId,Kind))], ((ConId,Type)) ))
                             lineno, context, tycon, kinded tyvars, constr 
                                    constr==NIL means exported abstractly. */

#define I_CLASS      269 /* snd :: ((Line, [((QConId,VarId))], ConId,
                                    [((VarId,Kind))], [((VarId,Type))]))
                            lineno, context, classname, 
                                      kinded tyvars, method sigs           */

#define I_VALUE      270 /* snd :: ((Line, VarId, Type))                   */

/*
   Top-level module entities.

   type Export = ?
*/
#define M_MODULE     280 /* snd :: ((ConId, [Export], 
                                     M_IMPORT_Q .. M_VALUE]))
                            module name, export spec, top level entities   */

#define M_IMPORT_Q   281 /* snd :: ((?,?)) */
#define M_IMPORT_UNQ 282 /* snd :: ((?,?)) */
#define M_TYCON      283 /* snd :: ((Line,?,?,?)) */
#define M_CLASS      284 /* snd :: ((Line,?,?,?)) */
#define M_INST       285 /* snd :: ((Line,?,?)) */
#define M_DEFAULT    286 /* snd :: ((Line,?)) */
#define M_FOREIGN_EX 289 /* snd :: ((Line,?,?,?,?)) */
#define M_FOREIGN_IM 290 /* snd :: ((Line,?,?,?,?)) */
#define M_VALUE      291 /* snd :: ? */




/* 
   Tagged tuples.
*/
#define ZTUP2        295          /* snd :: (Cell,Cell)                    */
#define ZTUP3        296          /* snd :: (Cell,(Cell,Cell))             */
#define ZTUP4        297          /* snd :: (Cell,(Cell,(Cell,Cell)))      */
#define ZTUP5        298       /* snd :: (Cell,(Cell,(Cell,(Cell,Cell))))  */



/* --------------------------------------------------------------------------
 * Special cell values.
 * ------------------------------------------------------------------------*/

#define TAG_SPEC_MIN 400
#define TAG_SPEC_MAX 428

#define isSpec(c) (TAG_SPEC_MIN<=(c) && (c)<=TAG_SPEC_MAX)

#define NONE         400          /* Dummy stub                            */
#define STAR         401          /* Representing the kind of types        */
#if TREX
#define ROW          402          /* Representing the kind of rows         */
#endif
#define WILDCARD     403          /* Wildcard pattern                      */
#define SKOLEM       404          /* Skolem constant                       */

#define DOTDOT       405          /* ".." in import/export list            */

#define NAME         406          /* whatIs code for isName                */
#define TYCON        407          /* whatIs code for isTycon               */
#define CLASS        408          /* whatIs code for isClass               */
#define MODULE       409          /* whatIs code for isModule              */
#define INSTANCE     410          /* whatIs code for isInst                */
#define TUPLE        411          /* whatIs code for tuple constructor     */
#define OFFSET       412          /* whatis code for offset                */
#define AP           413          /* whatIs code for application node      */
#define CHARCELL     414          /* whatIs code for isChar                */
#if TREX
#define EXT          415          /* whatIs code for isExt                 */
#endif

#define SIGDECL      416          /* Signature declaration                 */
#define FIXDECL      417          /* Fixity declaration                    */
#define FUNBIND      418          /* Function binding                      */
#define PATBIND      419          /* Pattern binding                       */

#define DATATYPE     420          /* Datatype type constructor             */
#define NEWTYPE      421          /* Newtype type constructor              */
#define SYNONYM      422          /* Synonym type constructor              */
#define RESTRICTSYN  423          /* Synonym with restricted scope         */

#define NODEPENDS    424          /* Stop calculation of deps in type check*/
#define PREDEFINED   425          /* Predefined name, not yet filled       */
#define TEXTCELL     426          /* whatIs code for isText                */
#define INVAR        427          /* whatIs code for isInventedVar         */
#define INDVAR       428          /* whatIs code for isInventedDictVar     */


/* --------------------------------------------------------------------------
 * Tuple data/type constructors:
 * ------------------------------------------------------------------------*/

extern Text ghcTupleText    ( Tycon );
extern Text ghcTupleText_n  ( Int );



#if TREX
#error TREX not supported
#define EXTMIN       301
#define isExt(c)     (EXTMIN<=(c) && (c)<OFFMIN)
#define extText(e)   tabExt[(e)-EXTMIN]
#define extField(c)  arg(fun(c))
#define extRow(c)    arg(c)

extern Text          DECTABLE(tabExt);
extern Ext           mkExt ( Text );
#else
#define mkExt(t) NIL
#endif

extern Module        findFakeModule ( Text t );
extern Tycon         addTupleTycon ( Int n );
extern Name          addWiredInBoxingTycon
                        ( String modNm, String typeNm, String constrNm,
                          Int rep, Kind kind );
extern Tycon         addWiredInEnumTycon 
                        ( String modNm, String typeNm, 
                          List /*of Text*/ constrs );

/* --------------------------------------------------------------------------
 * Offsets: (generic types/stack offsets)
 * ------------------------------------------------------------------------*/

#define OFF_MIN 1000
#define OFF_MAX 1999

#define isOffset(c)  (OFF_MIN<=(c) && (c)<=OFF_MAX)
#define offsetOf(c)  ((c)-OFF_MIN)
#define mkOffset(o)  (OFF_MIN+(o))


/* --------------------------------------------------------------------------
 * Modules:
 * ------------------------------------------------------------------------*/

#define MODULE_BASE_ADDR     5000000
#define MODULE_MAX_SIZE      900000
#define MODULE_INIT_SIZE     4

#ifdef DEBUG_STORAGE
extern struct strModule* generate_module_ref ( Cell );
#define module(mod)  (*generate_module_ref(mod))
#else
#define module(mod)   tabModule[(mod)-MODULE_BASE_ADDR]
#endif

#define mkModule(n)   (MODULE_BASE_ADDR+(n))
#define isModule(c)   (MODULE_BASE_ADDR<=(c)                  \
                       && (c)<MODULE_BASE_ADDR+tabModuleSz    \
                       && tabModule[(c)-MODULE_BASE_ADDR].inUse)


/* Import defns for the ObjectCode struct in Module. */
#include "object.h"

/* Import a machine-dependent definition of Time, for module timestamps. */
#include "machdep_time.h"

/* Under Haskell 1.3, the list of qualified imports is always a subset
 * of the list of unqualified imports.  For simplicity and flexibility,
 * we do not attempt to exploit this fact - when a module is imported
 * unqualified, it is added to both the qualified and unqualified
 * import lists.
 * Similarily, Haskell 1.3 does not allow a constructor to be imported
 * or exported without exporting the type it belongs to but the export
 * list is just a flat list of Texts (before static analysis) or
 * Tycons, Names and Classes (after static analysis).
 */
struct strModule {
   Bool   inUse;
   Name   nextFree;

   Text   text;        /* Name of this module                              */

   List   tycons;      /* Lists of top level objects ...                   */
   List   names;       /* (local defns + imports)                          */
   List   classes;
   List   exports;     /* [ Entity | (Entity, NIL|DOTDOT) ]                */

   List   qualImports; /* Qualified imports.                               */

   Bool   fake;        /* TRUE if module exists only via GHC primop        */
                       /* defn; usually FALSE                              */

   Cell   tree;        /* Parse tree for mod or iface                      */
   Bool   completed;   /* Fully loaded or just parsed?                     */
   Time   lastStamp;   /* Time of last parse                               */

   Bool   fromSrc;     /* is it from source ?                              */
   Text   srcExt;      /* if yes, ".lhs", ".hs", etc"                      */
   List   uses;        /* :: [CONID] -- names of mods imported by this one */

   Text   objName;     /* Name of the primary object code file.            */
   Int    objSize;     /* Size of the primary object code file.            */

   ObjectCode* object;        /* Primary object code for this module.      */
   ObjectCode* objectExtras;  /* And any extras it might need.             */
   List   objectExtraNames;   /* :: [Text] -- names of extras              */
};

extern struct strModule* tabModule;
extern Int               tabModuleSz;

extern Module currentModule;           /* Module currently being processed */
extern List   moduleGraph;             /* :: [GRP_REC | GRP_NONREC]        */
extern List   prelModules;             /* :: [CONID]                       */
extern List   targetModules;           /* :: [CONID]                       */


extern Bool         isValidModule   ( Module );
extern Module       newModule       ( Text );
extern Void         nukeModule      ( Module );
extern Module       findModule      ( Text );
extern Module       findModid       ( Cell );
extern Void         setCurrModule   ( Module );

extern void         addOTabName     ( Module,char*,void* );
extern void*        lookupOTabName  ( Module,char* );
extern char*        nameFromOPtr    ( void* );

extern void         addSection      ( Module,void*,void*,OSectionKind );
extern OSectionKind lookupSection   ( void* );
extern void*    lookupOExtraTabName ( char* sym );

#define isPrelude(m) (m==modulePrelude)

#define N_PRELUDE_SCRIPTS (combined ? 32 : 1)

/* --------------------------------------------------------------------------
 * Type constructor names:
 * ------------------------------------------------------------------------*/

#define TYCON_BASE_ADDR   2000000
#define TYCON_MAX_SIZE    900000
#define TYCON_INIT_SIZE   4

#ifdef DEBUG_STORAGE
extern struct strTycon* generate_tycon_ref ( Cell );
#define tycon(tc)    (*generate_tycon_ref(tc))
#else
#define tycon(tc)    tabTycon[(tc)-TYCON_BASE_ADDR]
#endif

#define isTycon(c)   (TYCON_BASE_ADDR<=(c)                        \
                      && (c)<TYCON_BASE_ADDR+tabTyconSz           \
                      && tabTycon[(c)-TYCON_BASE_ADDR].inUse      \
                      && tabTycon[(c)-TYCON_BASE_ADDR].tuple==-1)
#define isTuple(c)   (TYCON_BASE_ADDR<=(c)                        \
                      && (c)<TYCON_BASE_ADDR+tabTyconSz           \
                      && tabTycon[(c)-TYCON_BASE_ADDR].inUse      \
                      && tabTycon[(c)-TYCON_BASE_ADDR].tuple>=0)
#define tupleOf(n)   (tycon(n).tuple)

extern Tycon mkTuple ( Int );


struct strTycon {
    Bool   inUse;
    Name   nextFree;
    Text   text;
    Int    line;
    Module mod;                         /* module that defines it          */
    Int    tuple;                      /* tuple number, or -1 if not tuple */
    Int    arity;
    Kind   kind;                        /* kind (includes arity) of Tycon  */
    Cell   what;                        /* DATATYPE/SYNONYM/RESTRICTSYN... */
    Cell   defn;
    Name   conToTag;                    /* used in derived code            */
    Name   tagToCon;
    void*  itbl;                       /* For tuples, the info tbl pointer */
    Tycon  nextTyconHash;
};

extern struct strTycon* tabTycon;
extern Int              tabTyconSz;

extern Tycon newTycon     ( Text );
extern Tycon findTycon    ( Text );
extern Tycon addTycon     ( Tycon );
extern Tycon findQualTycon ( Cell );
extern Tycon addPrimTycon ( Text,Kind,Int,Cell,Cell );

#define isSynonym(h)    (isTycon(h) && tycon(h).what==SYNONYM)
#define isQualType(t)	(isPair(t) && fst(t)==QUAL)
#define mkPolyType(n,t) pair(POLYTYPE,pair(n,t))
#define isPolyType(t)   (isPair(t) && fst(t)==POLYTYPE)
#define isPolyOrQualType(t) (isPair(t) && (fst(t)==POLYTYPE || fst(t)==QUAL))
#define polySigOf(t)    fst(snd(t))
#define monotypeOf(t)   snd(snd(t))
#define bang(t)         ap(BANG,t)

extern Tycon findQualTyconWithoutConsultingExportList ( QualId q );

/* --------------------------------------------------------------------------
 * Globally defined name values:
 * ------------------------------------------------------------------------*/

#define NAME_BASE_ADDR    1000000
#define NAME_MAX_SIZE     900000
#define NAME_INIT_SIZE    4

#ifdef DEBUG_STORAGE
extern struct strName* generate_name_ref ( Cell );
#define name(nm)    (*generate_name_ref(nm))
#else
#define name(nm)    tabName[(nm)-NAME_BASE_ADDR]
#endif

#define mkName(n)   (NAME_BASE_ADDR+(n))
#define isName(c)   (NAME_BASE_ADDR<=(c)                   \
                     && (c)<NAME_BASE_ADDR+tabNameSz       \
                     && tabName[(c)-NAME_BASE_ADDR].inUse)

struct strName {
    Bool   inUse;
    Name   nextFree;
    Text   text;
    Int    line;
    Module mod;                         /* module that defines it          */
    Syntax syntax;
    Cell   parent; 
    Int    arity;
    Int    number;
    Cell   type;
    Cell   defn;
    Cell   stgVar;                                      /* really StgVar   */
    Text   callconv;                          /* for foreign import/export */
    void*  primop;                                      /* really StgPrim* */
    void*  itbl;                 /* For constructors, the info tbl pointer */
    Name   nextNameHash;
};

extern struct strName* tabName;
extern Int             tabNameSz;

extern int numNames (  Void  );

/* The number field in a name is used to distinguish various kinds of name:
 *   mfunNo(i) = code for member function, offset i
 *               members that are sole elements of dict use mfunNo(0)
 *               members of dicts with more than one elem use mfunNo(n), n>=1
 *   EXECNAME  = code for executable name (bytecodes or primitive)
 *   SELNAME   = code for selector function
 *   DFUNNAME  = code for dictionary builder or selector
 *   cfunNo(i) = code for data constructor
 *               datatypes with only one constructor uses cfunNo(0)
 *               datatypes with multiple constructors use cfunNo(n), n>=1
 */

#define EXECNAME        0
#define SELNAME         1
#define DFUNNAME        2
#define CFUNNAME        3

#define isSfun(n)       (name(n).number==SELNAME)
#define isDfun(n)       (name(n).number==DFUNNAME)

#define isCfun(n)       (name(n).number>=CFUNNAME)
#define cfunOf(n)       (name(n).number-CFUNNAME)
#define cfunNo(i)       ((i)+CFUNNAME)
#define hasCfun(cs)     (nonNull(cs) && isCfun(hd(cs)))

#define isMfun(n)       (name(n).number<0)
#define mfunOf(n)       ((-1)-name(n).number)
#define mfunNo(i)       ((-1)-(i))

extern Name   newName         ( Text,Cell );
extern Name   findName        ( Text );
extern Name   addName         ( Name );
extern Name   findQualName    ( Cell );
extern Name   addPrimCfun     ( Text,Int,Int,Cell );
extern Name   addPrimCfunREP  ( Text,Int,Int,Int );
extern Int    sfunPos         ( Name,Name );
extern Name   nameFromStgVar  ( Cell );
extern Name   jrsFindQualName ( Text,Text );

extern Name findQualNameWithoutConsultingExportList ( QualId q );

/* --------------------------------------------------------------------------
 * Type class values:
 * ------------------------------------------------------------------------*/

#define INST_BASE_ADDR     4000000
#define INST_MAX_SIZE      900000
#define INST_INIT_SIZE     4

#ifdef DEBUG_STORAGE
extern struct strInst* generate_inst_ref ( Cell );
#define inst(in)    (*generate_inst_ref(in))
#else
#define inst(in)    tabInst[(in)-INST_BASE_ADDR]
#endif

#define mkInst(n)   (INST_BASE_ADDR+(n))
#define instOf(c)   ((Int)((c)-INST_BASE_ADDR))
#define isInst(c)   (INST_BASE_ADDR<=(c)                   \
                     && (c)<INST_BASE_ADDR+tabInstSz       \
                     && tabInst[(c)-INST_BASE_ADDR].inUse)

struct strInst {
    Bool   inUse;
    Name   nextFree;
    Class  c;                           /* class C                         */
    Int    line;
    Module mod;                         /* module that defines it          */
    Kinds  kinds;                       /* Kinds of variables in head      */
    Cell   head;                        /* :: Pred                         */
    List   specifics;                   /* :: [Pred]                       */
    Int    numSpecifics;                /* length(specifics)               */
    List   implements;
    Name   builder;                     /* Dictionary constructor function */
};

extern struct strInst* tabInst;
extern Int             tabInstSz;

/* a predicate (an element :: Pred) is an application of a Class to one or
 * more type expressions
 */

#define CCLASS_BASE_ADDR   3000000
#define CCLASS_MAX_SIZE    900000
#define CCLASS_INIT_SIZE   4

#ifdef DEBUG_STORAGE
extern struct strClass* generate_cclass_ref ( Cell );
#define cclass(cl)   (*generate_cclass_ref(cl))
#else
#define cclass(cl)   tabClass[(cl)-CCLASS_BASE_ADDR]
#endif

#define mkClass(n)   (CCLASS_BASE_ADDR+(n))
#define isClass(c)   (CCLASS_BASE_ADDR<=(c)                   \
                      && (c)<CCLASS_BASE_ADDR+tabClassSz      \
                      && tabClass[(c)-CCLASS_BASE_ADDR].inUse)

struct strClass {
    Bool   inUse;
    Name   nextFree;
    Text   text;                        /* Name of class                   */
    Int    line;                        /* Line where declaration begins   */
    Module mod;                         /* module that declares it         */
    Int    level;                       /* Level in class hierarchy        */
    Int    arity;                       /* Number of arguments             */
    Kinds  kinds;                       /* Kinds of constructors in class  */
    List   fds;				/* Functional Dependencies	   */
    List   xfds;			/* Xpanded Functional Dependencies */
    Cell   head;                        /* Head of class                   */
    Name   dcon;                        /* Dictionary constructor function */
    List   supers;                      /* :: [Pred]                       */
    Int    numSupers;                   /* length(supers)                  */
    List   dsels;                       /* Superclass dictionary selectors */
    List   members;                     /* :: [Name]                       */
    Int    numMembers;                  /* length(members)                 */
    List   defaults;                    /* :: [Name]                       */
    List   instances;                   /* :: [Inst]                       */
};

extern struct strClass* tabClass;
extern Int              tabClassSz;

extern Class newClass      ( Text );
extern Class findClass     ( Text );
extern Class addClass      ( Class );
extern Class findQualClass ( Cell );
extern Inst  newInst       ( Void );
extern Inst  findFirstInst ( Tycon );
extern Inst  findNextInst  ( Tycon,Inst );
extern List  getAllKnownTyconsAndClasses ( void );
extern Class findQualClassWithoutConsultingExportList ( QualId q );

/* --------------------------------------------------------------------------
 * Character values:
 * ------------------------------------------------------------------------*/

/* I think this assumes that NUM_CHARS==256. */
#define CHARR_MIN    3000
#define CHARR_MAX    3255
#define isChar(c)    (CHARR_MIN<=(c) && (c)<=CHARR_MAX)
#define charOf(c)    ((Char)((c)-CHARR_MIN))
#define mkChar(c)    (CHARR_MIN+(((Cell)(c)) & 0xFF))
#define MAXCHARVAL   (NUM_CHARS-1)

/* --------------------------------------------------------------------------
 * Small Integer values:
 * ------------------------------------------------------------------------*/

#define SMALL_INT_MIN   100000
#define SMALL_INT_MAX   499999
#define SMALL_INT_ZERO  (1 + SMALL_INT_MIN/2 + SMALL_INT_MAX/2)
#define isSmall(c)      (SMALL_INT_MIN<=(c) && (c)<=SMALL_INT_MAX)
extern  Bool isInt      ( Cell );
extern  Int  intOf      ( Cell );
extern  Cell mkInt      ( Int );

/* --------------------------------------------------------------------------
 * Implementation of triples:
 * ------------------------------------------------------------------------*/

#define triple(x,y,z) pair(x,pair(y,z))
#define fst3(c)      fst(c)
#define snd3(c)      fst(snd(c))
#define thd3(c)      snd(snd(c))

/* --------------------------------------------------------------------------
 * Implementation of lists:
 * ------------------------------------------------------------------------*/

#define NIL              0
#define isNull(c)        ((c)==NIL)
#define nonNull(c)       (c)
#define cons(x,xs)       pair(x,xs)
#define singleton(x)     cons(x,NIL)
#define doubleton(x,y)   cons(x,cons(y,NIL))
#define tripleton(x,y,z) cons(x,cons(y,cons(z,NIL)))
#define hd(c)            fst(c)
#define tl(c)            snd(c)

extern  Int          length       ( List );
extern  List         appendOnto   ( List,List );    /* destructive     */
extern  List         dupOnto      ( List,List );
extern  List         dupList      ( List );
extern  List         revOnto      ( List, List );   /* destructive     */
#define rev(xs)      revOnto((xs),NIL)              /* destructive     */
#define reverse(xs)  revOnto(dupList(xs),NIL)       /* non-destructive */
extern  Cell         cellIsMember ( Cell,List );
extern  Cell         cellAssoc    ( Cell,List );
extern  Cell         cellRevAssoc ( Cell,List );
extern  Bool         eqList       ( List,List );
extern  Cell         varIsMember  ( Text,List );
extern  Name         nameIsMember ( Text,List );
extern  QualId       qualidIsMember ( QualId, List );
extern  Cell         intIsMember  ( Int,List );
extern  List         replicate    ( Int,Cell );
extern  List         diffList     ( List,List );    /* destructive     */
extern  List         deleteCell   ( List,Cell );    /* non-destructive */
extern  List         take         ( Int,List );     /* destructive     */
extern  List         splitAt      ( Int,List );     /* non-destructive */
extern  Cell         nth          ( Int,List );
extern  List         removeCell   ( Cell,List );    /* destructive     */
extern  List         dupListOnto  ( List,List );    /* non-destructive */ 
extern  List         nubList      ( List );         /* non-destructive */

/* The following macros provide `inline expansion' of some common ways of
 * traversing, using and modifying lists:
 *
 * N.B. We use the names _f, _a, _xs, Zs, in an attempt to avoid clashes
 *      with identifiers used elsewhere.
 */

#define mapBasic(_init,_step)           {List Zs=(_init);\
                                         for(;nonNull(Zs);Zs=tl(Zs))  \
                                         _step;}
#define mapModify(_init,_step)          mapBasic(_init,hd(Zs)=_step)

#define mapProc(_f,_xs)                 mapBasic(_xs,_f(hd(Zs)))
#define map1Proc(_f,_a,_xs)             mapBasic(_xs,_f(_a,hd(Zs)))
#define map2Proc(_f,_a,_b,_xs)          mapBasic(_xs,_f(_a,_b,hd(Zs)))
#define map3Proc(_f,_a,_b,_c,_xs)       mapBasic(_xs,_f(_a,_b,_c,hd(Zs)))
#define map4Proc(_f,_a,_b,_c,_d,_xs)    mapBasic(_xs,_f(_a,_b,_c,_d,hd(Zs)))

#define mapOver(_f,_xs)                 mapModify(_xs,_f(hd(Zs)))
#define map1Over(_f,_a,_xs)             mapModify(_xs,_f(_a,hd(Zs)))
#define map2Over(_f,_a,_b,_xs)          mapModify(_xs,_f(_a,_b,hd(Zs)))
#define map3Over(_f,_a,_b,_c,_xs)       mapModify(_xs,_f(_a,_b,_c,hd(Zs)))
#define map4Over(_f,_a,_b,_c,_d,_xs)    mapModify(_xs,_f(_a,_b,_c,_d,hd(Zs)))

/* This is just what you want for functions with accumulating parameters */
#define mapAccum(_f,_acc,_xs)           mapBasic(_xs,_acc=_f(_acc,hd(Zs)))
#define map1Accum(_f,_acc,_a,_xs)       mapBasic(_xs,_acc=_f(_acc,_a,hd(Zs)))
#define map2Accum(_f,_acc,_a,_b,_xs)    mapBasic(_xs,_acc=_f(_acc,_a,_b,hd(Zs)))
#define map3Accum(_f,_acc,_a,_b,_c,_xs) mapBasic(_xs,_acc=_f(_acc,_a,_b,_c,hd(Zs)))


/* --------------------------------------------------------------------------
 * Strongly-typed lists (z-lists) and tuples (experimental)
 * ------------------------------------------------------------------------*/

typedef Cell ZPair;
typedef Cell ZTriple;
typedef Cell Z4Ble;
typedef Cell Z5Ble;

#define isZPair(c) (whatIs((c))==ZTUP2)

extern Cell zpair    ( Cell x1, Cell x2 );
extern Cell zfst     ( Cell zpair );
extern Cell zsnd     ( Cell zpair );

extern Cell ztriple  ( Cell x1, Cell x2, Cell x3 );
extern Cell zfst3    ( Cell zpair );
extern Cell zsnd3    ( Cell zpair );
extern Cell zthd3    ( Cell zpair );

extern Cell z4ble    ( Cell x1, Cell x2, Cell x3, Cell x4 );
extern Cell zsel14   ( Cell zpair );
extern Cell zsel24   ( Cell zpair );
extern Cell zsel34   ( Cell zpair );
extern Cell zsel44   ( Cell zpair );

extern Cell z5ble    ( Cell x1, Cell x2, Cell x3, Cell x4, Cell x5 );
extern Cell zsel15   ( Cell zpair );
extern Cell zsel25   ( Cell zpair );
extern Cell zsel35   ( Cell zpair );
extern Cell zsel45   ( Cell zpair );
extern Cell zsel55   ( Cell zpair );

extern Cell unap     ( int tag, Cell c );


/* --------------------------------------------------------------------------
 * Implementation of function application nodes:
 * ------------------------------------------------------------------------*/

#define ap(f,x)      pair(f,x)
#define ap1(f,x)     ap(f,x)
#define ap2(f,x,y)   ap(ap(f,x),y)
#define ap3(f,x,y,z) ap(ap(ap(f,x),y),z)
#define fun(c)       fst(c)
#define arg(c)       snd(c)
#define isAp(c)      (isPair(c) && !isTag(fst(c)))

extern  Cell         getHead     ( Cell );
extern  List         getArgs     ( Cell );
extern  Cell         nthArg      ( Int,Cell );
extern  Int          numArgs     ( Cell );
extern  Cell         applyToArgs ( Cell,List );
extern  Int          argCount;

/* --------------------------------------------------------------------------
 * Stack implementation:
 *
 * NB: Use of macros makes order of evaluation hard to predict.
 *     For example, "push(1+pop());" doesn't increment TOS.
 * ------------------------------------------------------------------------*/

extern  Cell cellStack[];
extern  StackPtr sp;

#define clearStack() sp=(-1)
#define stackEmpty() (sp==(-1))
#define stack(p)     cellStack[p]
#define chkStack(n)  if (sp>=NUM_STACK-(n)) hugsStackOverflow()
#define push(c)      do { chkStack(1); onto(c); } while (0)
#define onto(c)      stack(++sp)=(c);
#define pop()        stack(sp--)
#define drop()       sp--
#define top()        stack(sp)
#define pushed(n)    stack(sp-(n))
#define topfun(f)    top()=ap((f),top())
#define toparg(x)    top()=ap(top(),(x))

extern  Void hugsStackOverflow ( Void );

#if SYMANTEC_C
#include <Memory.h>
#define STACK_HEADROOM 16384
#define STACK_CHECK if (StackSpace() <= STACK_HEADROOM) \
		      internal("Macintosh function parameter stack overflow.");
#else
#define STACK_CHECK
#endif

/* --------------------------------------------------------------------------
 * Misc:
 * ------------------------------------------------------------------------*/

extern  Void   setLastExpr          ( Cell );
extern  Cell   getLastExpr          ( Void );
extern  List   addTyconsMatching    ( String,List );
extern  List   addNamesMatching     ( String,List );

extern  Tycon  findTyconInAnyModule ( Text t );
extern  Class  findClassInAnyModule ( Text t );
extern  Name   findNameInAnyModule  ( Text t );

extern  Void   print                ( Cell, Int );
extern  void   dumpTycon            ( Int t );
extern  void   dumpName             ( Int n );
extern  void   dumpClass            ( Int c );
extern  void   dumpInst             ( Int i );
extern  void   locateSymbolByName   ( Text t );

/*-------------------------------------------------------------------------*/
