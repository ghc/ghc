
/* --------------------------------------------------------------------------
 * Definitions for substitution data structure and operations.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: subst.h,v $
 * $Revision: 1.6 $
 * $Date: 1999/11/17 16:57:50 $
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
extern Kind  simpleKind         Args((Int));
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

/*-------------------------------------------------------------------------*/
