%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[DsExpr]{Matching expressions (Exprs)}

\begin{code}
#include "HsVersions.h"

module DsExpr ( dsExpr ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Pretty
import Outputable

import AbsSyn		-- the stuff being desugared
import PlainCore	-- the output of desugaring;
			-- importing this module also gets all the
			-- CoreSyn utility functions
import DsMonad		-- the monadery used in the desugarer

import AbsPrel		( mkTupleTy, unitTy, nilDataCon, consDataCon,
			  charDataCon, charTy,
			  mkFunTy, mkBuild -- LATER: , foldrId
#ifdef DPH
			 ,fromDomainId, toDomainId
#endif {- Data Parallel Haskell -}
			)
import PrimKind		( PrimKind(..) ) -- rather ugly import *** ToDo???
import AbsUniType	( alpha, alpha_tv, beta, beta_tv, splitType,
			  splitTyArgs, mkTupleTyCon, mkTyVarTy, mkForallTy,
			  kindFromType, maybeBoxedPrimType,
			  TyVarTemplate, TyCon, Arity(..), Class,
			  TauType(..), UniType
			)
import BasicLit		( mkMachInt, BasicLit(..) )
import CmdLineOpts	( GlobalSwitch(..), SwitchResult, switchIsOn )
import CostCentre	( mkUserCC )
import DsBinds		( dsBinds )
import DsCCall		( dsCCall )
import DsListComp	( dsListComp )
import DsUtils		( mkCoAppDs, mkCoConDs, mkCoPrimDs, dsExprToAtom )
import Id
import IdEnv
import IdInfo
import Match		( matchWrapper )
import Maybes		( Maybe(..) )
import TaggedCore	( TaggedBinder(..), unTagBinders )
import TyVarEnv
import Util

#ifdef DPH
import DsParZF		( dsParallelZF )
#endif {- Data Parallel Haskell -}
\end{code}

The funny business to do with variables is that we look them up in the
Id-to-Id and Id-to-Id maps that the monadery is carrying
around; if we get hits, we use the value accordingly.

%************************************************************************
%*									*
\subsection[DsExpr-vars-and-cons]{Variables and constructors}
%*									*
%************************************************************************

\begin{code}
dsExpr :: TypecheckedExpr -> DsM PlainCoreExpr

dsExpr (Var var) = dsApp (Var var) []
\end{code}

%************************************************************************
%*									*
\subsection[DsExpr-literals]{Literals}
%*									*
%************************************************************************

We give int/float literals type Integer and Rational, respectively.
The typechecker will (presumably) have put \tr{from{Integer,Rational}s}
around them.

ToDo: put in range checks for when converting "i"
(or should that be in the typechecker?)

For numeric literals, we try to detect there use at a standard type
(Int, Float, etc.) are directly put in the right constructor.
[NB: down with the @App@ conversion.]
Otherwise, we punt, putting in a "NoRep" Core literal (where the
representation decisions are delayed)...

See also below where we look for @DictApps@ for \tr{plusInt}, etc.

\begin{code}
dsExpr (Lit (StringLit s))
  | _NULL_ s
  = returnDs ( CoCon nilDataCon [charTy] [] )

  | _LENGTH_ s == 1
  = let
	the_char = CoCon charDataCon [] [CoLitAtom (MachChar (_HEAD_ s))] 
	the_nil  = CoCon nilDataCon  [charTy] []
    in
    mkCoConDs consDataCon [charTy] [the_char, the_nil]

-- "_" => build (\ c n -> c 'c' n)	-- LATER

-- "str" ==> build (\ c n -> foldr charTy T c n "str")

{- LATER:
dsExpr (Lit (StringLit str)) =
    newTyVarsDs [alpha_tv]		`thenDs` \ [new_tyvar] ->
    let
 	new_ty = mkTyVarTy new_tyvar
    in
    newSysLocalsDs [ 
		charTy `mkFunTy` (new_ty `mkFunTy` new_ty),
		new_ty,
		       mkForallTy [alpha_tv]
			       ((charTy `mkFunTy` (alpha `mkFunTy` alpha))
			       	        `mkFunTy` (alpha `mkFunTy` alpha))
		]			`thenDs` \ [c,n,g] ->
     returnDs (mkBuild charTy new_tyvar c n g (
	foldl CoApp
	  (CoTyApp (CoTyApp (CoVar foldrId) charTy) new_ty) *** ensure non-prim type ***
   	  [CoVarAtom c,CoVarAtom n,CoLitAtom (NoRepStr str)]))
-}

-- otherwise, leave it as a NoRepStr;
-- the Core-to-STG pass will wrap it in an application of "unpackCStringId".

dsExpr (Lit (StringLit str))
  = returnDs (CoLit (NoRepStr str))

dsExpr (Lit (LitLitLit s ty))
  = returnDs ( CoCon data_con [] [CoLitAtom (MachLitLit s kind)] )
  where
    (data_con, kind)
      = case (maybeBoxedPrimType ty) of
	  Nothing
	    -> error ("ERROR: ``literal-literal'' not a single-constructor type: "++ _UNPK_ s ++"; type: "++(ppShow 80 (ppr PprDebug ty)))
	  Just (boxing_data_con, prim_ty)
	    -> (boxing_data_con, kindFromType prim_ty)

dsExpr (Lit (IntLit i))
  = returnDs (CoLit (NoRepInteger i))

dsExpr (Lit (FracLit r))
  = returnDs (CoLit (NoRepRational r))

-- others where we know what to do:

dsExpr (Lit (IntPrimLit i))
  = if (i >= toInteger minInt && i <= toInteger maxInt) then
    	returnDs (CoLit (mkMachInt i))
    else
	error ("ERROR: Int constant " ++ show i ++ out_of_range_msg)

dsExpr (Lit (FloatPrimLit f))
  = returnDs (CoLit (MachFloat f))
    -- ToDo: range checking needed!

dsExpr (Lit (DoublePrimLit d))
  = returnDs (CoLit (MachDouble d))
    -- ToDo: range checking needed!

dsExpr (Lit (CharLit c))
  = returnDs ( CoCon charDataCon [] [CoLitAtom (MachChar c)] )

dsExpr (Lit (CharPrimLit c))
  = returnDs (CoLit (MachChar c))

dsExpr (Lit (StringPrimLit s))
  = returnDs (CoLit (MachStr s))

-- end of literals magic. --

dsExpr expr@(Lam a_Match)
  = let
	error_msg = "%L" --> "pattern-matching failed in lambda"
    in
    matchWrapper LambdaMatch [a_Match] error_msg `thenDs` \ (binders, matching_code) ->
    returnDs ( mkCoLam binders matching_code )

dsExpr expr@(App e1 e2) = dsApp expr []

dsExpr expr@(OpApp e1 op e2) = dsApp expr []
\end{code}

Operator sections.  At first it looks as if we can convert
\begin{verbatim}
	(expr op)
\end{verbatim}
to 
\begin{verbatim}
	\x -> op expr x
\end{verbatim}

But no!  expr might be a redex, and we can lose laziness badly this
way.  Consider
\begin{verbatim}
	map (expr op) xs
\end{verbatim}
for example.  So we convert instead to
\begin{verbatim}
	let y = expr in \x -> op y x
\end{verbatim}
If \tr{expr} is actually just a variable, say, then the simplifier
will sort it out.

\begin{code}
dsExpr (SectionL expr op)
  = dsExpr op			`thenDs` \ core_op ->
    dsExpr expr			`thenDs` \ core_expr ->
    dsExprToAtom core_expr	( \ y_atom ->

    -- for the type of x, we need the type of op's 2nd argument
    let
	x_ty  =	case (splitType (typeOfCoreExpr core_op)) of { (_, _, tau_ty) ->
		case (splitTyArgs tau_ty)		  of {
		  ((_:arg2_ty:_), _) -> arg2_ty;
		  _ -> panic "dsExpr:SectionL:arg 2 ty"--++(ppShow 80 (ppAboves [ppr PprDebug (typeOfCoreExpr core_op), ppr PprDebug tau_ty]))
		}}
    in
    newSysLocalDs x_ty		`thenDs` \ x_id ->
    returnDs ( mkCoLam [x_id] (CoApp (CoApp core_op y_atom) (CoVarAtom x_id)) ))

-- dsExpr (SectionR op expr)	-- \ x -> op x expr
dsExpr (SectionR op expr)
  = dsExpr op			`thenDs` \ core_op ->
    dsExpr expr			`thenDs` \ core_expr ->
    dsExprToAtom core_expr	(\ y_atom ->

    -- for the type of x, we need the type of op's 1st argument
    let
	x_ty  =	case (splitType (typeOfCoreExpr core_op)) of { (_, _, tau_ty) ->
		case (splitTyArgs tau_ty)		  of {
		  ((arg1_ty:_), _) -> arg1_ty;
		  _ -> panic "dsExpr:SectionR:arg 1 ty"--++(ppShow 80 (ppAboves [ppr PprDebug (typeOfCoreExpr core_op), ppr PprDebug tau_ty]))
		}}
    in
    newSysLocalDs x_ty		`thenDs` \ x_id ->
    returnDs ( mkCoLam [x_id] (CoApp (CoApp core_op (CoVarAtom x_id)) y_atom) ))

dsExpr (CCall label args may_gc is_asm result_ty)
  = mapDs dsExpr args		`thenDs` \ core_args ->
    dsCCall label core_args may_gc is_asm result_ty
	-- dsCCall does all the unboxification, etc.

dsExpr (SCC cc expr)
  = dsExpr expr			`thenDs` \ core_expr ->
    getModuleAndGroupDs		`thenDs` \ (mod_name, group_name) ->
    returnDs ( CoSCC (mkUserCC cc mod_name group_name) core_expr)

dsExpr expr@(Case discrim matches)
  = dsExpr discrim		   `thenDs` \ core_discrim ->
    let
	error_msg = "%C" --> "pattern-matching failed in case"
    in
    matchWrapper CaseMatch matches error_msg `thenDs` \ ([discrim_var], matching_code) ->
    returnDs ( mkCoLetAny (CoNonRec discrim_var core_discrim) matching_code )

dsExpr (ListComp expr quals)
  = dsExpr expr `thenDs` \ core_expr ->
    dsListComp core_expr quals

dsExpr (Let binds expr)
  = dsBinds binds	`thenDs` \ core_binds ->
    dsExpr expr		`thenDs` \ core_expr ->
    returnDs ( mkCoLetsAny core_binds core_expr )

dsExpr (ExplicitList _)	= panic "dsExpr:ExplicitList -- not translated"

dsExpr (ExplicitListOut ty xs)
  = case xs of
      []     -> returnDs ( CoCon nilDataCon [ty] [] )
      (y:ys) ->
	dsExpr y			    `thenDs` \ core_hd  ->
	dsExpr (ExplicitListOut ty ys)  `thenDs` \ core_tl  ->
	mkCoConDs consDataCon [ty] [core_hd, core_tl]

dsExpr (ExplicitTuple expr_list)
  = mapDs dsExpr expr_list	  `thenDs` \ core_exprs  ->
    mkCoConDs (mkTupleCon (length expr_list))
	      (map typeOfCoreExpr core_exprs)
	      core_exprs

dsExpr (ExprWithTySig expr sig) = panic "dsExpr: ExprWithTySig"

dsExpr (If guard_expr then_expr else_expr)
  = dsExpr guard_expr	`thenDs` \ core_guard ->
    dsExpr then_expr	`thenDs` \ core_then ->
    dsExpr else_expr	`thenDs` \ core_else ->
    returnDs (mkCoreIfThenElse core_guard core_then core_else)

dsExpr (ArithSeqIn info) = panic "dsExpr.ArithSeqIn"

dsExpr (ArithSeqOut expr (From from))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    mkCoAppDs expr2 from2

dsExpr (ArithSeqOut expr (FromTo from two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr two		  `thenDs` \ two2 ->
    mkCoAppDs expr2 from2 `thenDs` \ app1 ->
    mkCoAppDs app1  two2

dsExpr (ArithSeqOut expr (FromThen from thn))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr thn		  `thenDs` \ thn2 ->
    mkCoAppDs expr2 from2 `thenDs` \ app1 ->
    mkCoAppDs app1  thn2

dsExpr (ArithSeqOut expr (FromThenTo from thn two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr thn		  `thenDs` \ thn2 ->
    dsExpr two		  `thenDs` \ two2 ->
    mkCoAppDs expr2 from2 `thenDs` \ app1 ->
    mkCoAppDs app1  thn2  `thenDs` \ app2 ->
    mkCoAppDs app2  two2

#ifdef DPH
dsExpr (ParallelZF expr quals)
  = dsParallelZF expr  quals

dsExpr (ExplicitPodIn _) 
  = panic "dsExpr:ExplicitPodIn -- not translated"

dsExpr (ExplicitPodOut _ _)
  = panic "dsExpr:ExplicitPodOut should remove this."

dsExpr (ExplicitProcessor exprs expr)
  = mapDs dsExpr exprs		`thenDs` \ core_exprs	->
    dsExpr expr			`thenDs` \ core_expr ->
    mkCoConDs (mkProcessorCon (length exprs))
	      ((map typeOfCoreExpr core_exprs)++[typeOfCoreExpr core_expr])
	      (core_exprs++[core_expr])
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
dsExpr (TyLam tyvars expr)
  = dsExpr expr `thenDs` \ core_expr ->
    returnDs( foldr CoTyLam core_expr tyvars)

dsExpr expr@(TyApp e tys) = dsApp expr []
\end{code}

@DictLam@ and @DictApp@ turn into the regular old things.
(OLD:) @DictFunApp@ also becomes a curried application, albeit slightly more
complicated; reminiscent of fully-applied constructors.
\begin{code}
dsExpr (DictLam dictvars expr)
  = dsExpr expr `thenDs` \ core_expr ->
    returnDs( mkCoLam dictvars core_expr )

------------------

dsExpr expr@(DictApp e dicts)	-- becomes a curried application
  = dsApp expr []
\end{code}

@SingleDicts@ become @Locals@; @Dicts@ turn into tuples, unless
of length 0 or 1.
@ClassDictLam dictvars methods expr@ is ``the opposite'':
\begin{verbatim}
\ x -> case x of ( dictvars-and-methods-tuple ) -> expr
\end{verbatim}
\begin{code}
dsExpr (SingleDict dict)	-- just a local
  = lookupEnvWithDefaultDs dict (CoVar dict)

dsExpr (Dictionary dicts methods)
  = -- hey, these things may have been substituted away...
    zipWithDs lookupEnvWithDefaultDs
	      dicts_and_methods dicts_and_methods_exprs
			`thenDs` \ core_d_and_ms ->

    (case num_of_d_and_ms of
      0 -> returnDs cocon_unit -- unit

      1 -> returnDs (head core_d_and_ms) -- just a single Id

      _ ->	    -- tuple 'em up
	   mkCoConDs (mkTupleCon num_of_d_and_ms)
		     (map typeOfCoreExpr core_d_and_ms)
		     core_d_and_ms 
    )
  where
    dicts_and_methods	    = dicts ++ methods
    dicts_and_methods_exprs = map CoVar dicts_and_methods
    num_of_d_and_ms	    = length dicts_and_methods

dsExpr (ClassDictLam dicts methods expr)
  = dsExpr expr		`thenDs` \ core_expr ->
    case num_of_d_and_ms of
	0 -> newSysLocalDs unitTy `thenDs` \ new_x ->
	     returnDs (CoLam [new_x] core_expr)

	1 -> -- no untupling
	    returnDs (CoLam dicts_and_methods core_expr)

	_ ->				-- untuple it
	    newSysLocalDs tuple_ty `thenDs` \ new_x ->
	    returnDs (
	      CoLam [new_x]
		(CoCase (CoVar new_x)
		    (CoAlgAlts
			[(tuple_con, dicts_and_methods, core_expr)]
			CoNoDefault)))
  where
    dicts_and_methods	    = dicts ++ methods
    num_of_d_and_ms	    = length dicts_and_methods
    tuple_ty		    = mkTupleTy num_of_d_and_ms (map getIdUniType dicts_and_methods)
    tuple_tycon		    = mkTupleTyCon num_of_d_and_ms
    tuple_con		    = mkTupleCon   num_of_d_and_ms

cocon_unit = CoCon (mkTupleCon 0) [] [] -- out here to avoid CAF (sigh)
out_of_range_msg			-- ditto
  = " out of range: [" ++ show minInt ++ ", " ++ show maxInt ++ "]\n"
\end{code}

%--------------------------------------------------------------------

@(dsApp e [t_1,..,t_n, e_1,..,e_n])@ returns something with the same
value as:
\begin{verbatim}
e t_1 ... t_n  e_1 .. e_n
\end{verbatim}

We're doing all this so we can saturate constructors (as painlessly as
possible).

\begin{code}
data DsCoreArg
  = DsTypeArg UniType
  | DsValArg  PlainCoreExpr

dsApp :: TypecheckedExpr	-- expr to desugar
      -> [DsCoreArg]		-- accumulated ty/val args: NB:
      -> DsM PlainCoreExpr	-- final result

dsApp (App e1 e2) args
  = dsExpr e2			`thenDs` \ core_e2 ->
    dsApp  e1 (DsValArg core_e2 : args)

dsApp (OpApp e1 op e2) args
  = dsExpr e1			`thenDs` \ core_e1 ->
    dsExpr e2			`thenDs` \ core_e2 ->
    dsApp  op (DsValArg core_e1 : DsValArg core_e2 : args)

dsApp (DictApp expr dicts) args
  =	-- now, those dicts may have been substituted away...
    zipWithDs lookupEnvWithDefaultDs dicts (map CoVar dicts)
				`thenDs` \ core_dicts ->
    dsApp expr (map DsValArg core_dicts ++ args)

dsApp (TyApp expr tys) args
  = dsApp expr (map DsTypeArg tys ++ args)

-- we might should look out for SectionLs, etc., here, but we don't

dsApp (Var v) args
  = lookupEnvDs v	`thenDs` \ maybe_expr ->
    case maybe_expr of
      Just expr -> apply_to_args expr args

      Nothing -> -- we're only saturating constructors and PrimOps
	case getIdUnfolding v of
	  GeneralForm _ _ the_unfolding EssentialUnfolding 
	    -> do_unfold nullTyVarEnv nullIdEnv (unTagBinders the_unfolding) args

	  _ -> apply_to_args (CoVar v) args


dsApp anything_else args
  = dsExpr anything_else	`thenDs` \ core_expr ->
    apply_to_args core_expr args

-- a DsM version of applyToArgs:
apply_to_args :: PlainCoreExpr -> [DsCoreArg] -> DsM PlainCoreExpr

apply_to_args fun [] = returnDs fun

apply_to_args fun (DsValArg expr : args)
  = mkCoAppDs fun expr	`thenDs` \ fun2 ->
    apply_to_args fun2 args

apply_to_args fun (DsTypeArg ty : args)
  = apply_to_args (mkCoTyApp fun ty) args
\end{code}

\begin{code}
do_unfold ty_env val_env (CoTyLam tyvar body) (DsTypeArg ty : args)
  = do_unfold (addOneToTyVarEnv ty_env tyvar ty) val_env body args

do_unfold ty_env val_env (CoLam [] body) args
  = do_unfold ty_env val_env body args

do_unfold ty_env val_env (CoLam (binder:binders) body) (DsValArg expr : args)
  = dsExprToAtom expr (\ arg_atom ->
	    do_unfold ty_env (addOneToIdEnv val_env binder (atomToExpr arg_atom)) (CoLam binders body) args
    )

do_unfold ty_env val_env body args
  = 	-- Clone the remaining part of the template
    uniqSMtoDsM (substCoreExprUS val_env ty_env body)	`thenDs` \ body' ->

	-- Apply result to remaining arguments
    apply_to_args body' args
\end{code}
