
/* --------------------------------------------------------------------------
 * STG syntax
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: backend.h,v $
 * $Revision: 1.5 $
 * $Date: 1999/10/15 21:41:02 $
 * ------------------------------------------------------------------------*/

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

extern int stgConTag  ( StgDiscr d );
extern void* stgConInfo ( StgDiscr d );
extern int stgDiscrTag( StgDiscr d );

/* --------------------------------------------------------------------------
 * Utility functions for manipulating STG syntax trees.
 * ------------------------------------------------------------------------*/

extern List    makeArgs      ( Int );
extern StgExpr makeStgLambda ( List args,  StgExpr body );
extern StgExpr makeStgApp    ( StgVar fun, List args );
extern StgExpr makeStgLet    ( List binds, StgExpr body );
extern StgExpr makeStgIf     ( StgExpr cond, StgExpr e1, StgExpr e2 );
extern Bool    isStgVar      ( StgRhs rhs );
extern Bool    isAtomic      ( StgRhs rhs );
extern StgVar  mkStgVar      ( StgRhs rhs, Cell info );

extern Int     stgSize       ( StgExpr e );

#define mkStgRep(c) mkChar(c)

/*-------------------------------------------------------------------------*/




extern Void  cgBinds       Args((StgRhs));
extern void* closureOfVar  Args((StgVar));
extern char* lookupHugsName Args((void*));



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

extern Void optimiseBind Args((StgVar));




Void printStg( FILE *fp, StgVar b);
            
#if DEBUG_PRINTER
extern Void ppStg        ( StgVar v );
extern Void ppStgExpr    ( StgExpr e );
extern Void ppStgRhs     ( StgRhs rhs );
extern Void ppStgAlts    ( List alts );
extern Void ppStgPrimAlts( List alts );
extern Void ppStgVars    ( List vs );
#endif


extern List liftBinds( List binds );
extern Void liftControl ( Int what );

extern StgExpr substExpr ( List sub, StgExpr e );
extern StgExpr zubstExpr ( List sub, StgExpr e );

extern List freeVarsBind Args((List, StgVar));
extern Void optimiseBind Args((StgVar));

#ifdef CRUDE_PROFILING
extern void cp_init ( void );
extern void cp_enter ( Cell /*StgVar*/ );
extern void cp_bill_words ( int );
extern void cp_bill_insns ( int );
extern void cp_show ( void );
#endif
