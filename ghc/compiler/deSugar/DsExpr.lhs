%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[DsExpr]{Matching expressions (Exprs)}

\begin{code}
module DsExpr ( dsExpr ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsBinds (dsBinds )

import HsSyn		( failureFreePat,
			  HsExpr(..), OutPat(..), HsLit(..), ArithSeqInfo(..),
			  Stmt(..), DoOrListComp(..), Match(..), HsBinds, HsType, Fixity,
			  GRHSsAndBinds
			)
import TcHsSyn		( TypecheckedHsExpr, TypecheckedHsBinds,
			  TypecheckedRecordBinds, TypecheckedPat,
			  TypecheckedStmt,
			  maybeBoxedPrimType

			)
import CoreSyn

import DsMonad
import DsCCall		( dsCCall )
import DsListComp	( dsListComp )
import DsUtils		( mkAppDs, mkConDs, dsExprToAtomGivenTy,
			  mkErrorAppDs, showForErr, DsCoreArg
			)
import Match		( matchWrapper )

import CoreUtils	( coreExprType, mkCoreIfThenElse )
import CostCentre	( mkUserCC )
import FieldLabel	( FieldLabel )
import Id		( dataConTyCon, dataConArgTys, dataConFieldLabels,
			  recordSelectorFieldLabel, Id
			)
import Literal		( mkMachInt, Literal(..) )
import Name		( Name{--O only-} )
import PrelVals		( rEC_CON_ERROR_ID, rEC_UPD_ERROR_ID )
import TyCon		( isNewTyCon )
import Type		( splitFunTys, typePrimRep, mkTyConApp,
			  splitAlgTyConApp, splitTyConApp_maybe,
			  splitAppTy, Type
			)
import TysWiredIn	( tupleCon, nilDataCon, consDataCon, listTyCon, mkListTy,
			  charDataCon, charTy
			)
import TyVar		( GenTyVar{-instance Eq-} )
import Maybes		( maybeToBool )
import Util		( zipEqual )
import Outputable

mk_nil_con ty = mkCon nilDataCon [ty] []  -- micro utility...
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
dsExpr :: TypecheckedHsExpr -> DsM CoreExpr

dsExpr e@(HsVar var) = dsId var
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
dsExpr (HsLitOut (HsString s) _)
  | _NULL_ s
  = returnDs (mk_nil_con charTy)

  | _LENGTH_ s == 1
  = let
	the_char = mkCon charDataCon [] [LitArg (MachChar (_HEAD_ s))]
	the_nil  = mk_nil_con charTy
    in
    mkConDs consDataCon [TyArg charTy, VarArg the_char, VarArg the_nil]

-- "_" => build (\ c n -> c 'c' n)	-- LATER

-- "str" ==> build (\ c n -> foldr charTy T c n "str")

{- LATER:
dsExpr (HsLitOut (HsString str) _)
  = newTyVarsDs [alphaTyVar]		`thenDs` \ [new_tyvar] ->
    let
 	new_ty = mkTyVarTy new_tyvar
    in
    newSysLocalsDs [
		charTy `mkFunTy` (new_ty `mkFunTy` new_ty),
		new_ty,
		       mkForallTy [alphaTyVar]
			       ((charTy `mkFunTy` (alphaTy `mkFunTy` alphaTy))
			       	        `mkFunTy` (alphaTy `mkFunTy` alphaTy))
		]			`thenDs` \ [c,n,g] ->
     returnDs (mkBuild charTy new_tyvar c n g (
	foldl App
	  (CoTyApp (CoTyApp (Var foldrId) charTy) new_ty) *** ensure non-prim type ***
   	  [VarArg c,VarArg n,LitArg (NoRepStr str)]))
-}

-- otherwise, leave it as a NoRepStr;
-- the Core-to-STG pass will wrap it in an application of "unpackCStringId".

dsExpr (HsLitOut (HsString str) _)
  = returnDs (Lit (NoRepStr str))

dsExpr (HsLitOut (HsLitLit s) ty)
  = returnDs ( mkCon data_con [] [LitArg (MachLitLit s kind)] )
  where
    (data_con, kind)
      = case (maybeBoxedPrimType ty) of
	  Just (boxing_data_con, prim_ty)
	    -> (boxing_data_con, typePrimRep prim_ty)
	  Nothing
	    -> pprPanic "ERROR: ``literal-literal'' not a single-constructor type: "
			(hcat [ptext s, text "; type: ", ppr ty])

dsExpr (HsLitOut (HsInt i) ty)
  = returnDs (Lit (NoRepInteger i ty))

dsExpr (HsLitOut (HsFrac r) ty)
  = returnDs (Lit (NoRepRational r ty))

-- others where we know what to do:

dsExpr (HsLitOut (HsIntPrim i) _)
  = if (i >= toInteger minInt && i <= toInteger maxInt) then
    	returnDs (Lit (mkMachInt i))
    else
	error ("ERROR: Int constant " ++ show i ++ out_of_range_msg)

dsExpr (HsLitOut (HsFloatPrim f) _)
  = returnDs (Lit (MachFloat f))
    -- ToDo: range checking needed!

dsExpr (HsLitOut (HsDoublePrim d) _)
  = returnDs (Lit (MachDouble d))
    -- ToDo: range checking needed!

dsExpr (HsLitOut (HsChar c) _)
  = returnDs ( mkCon charDataCon [] [LitArg (MachChar c)] )

dsExpr (HsLitOut (HsCharPrim c) _)
  = returnDs (Lit (MachChar c))

dsExpr (HsLitOut (HsStringPrim s) _)
  = returnDs (Lit (MachStr s))

-- end of literals magic. --

dsExpr expr@(HsLam a_Match)
  = matchWrapper LambdaMatch [a_Match] "lambda"	`thenDs` \ (binders, matching_code) ->
    returnDs ( mkValLam binders matching_code )

dsExpr expr@(HsApp fun arg)      
  = dsExpr fun		`thenDs` \ core_fun ->
    dsExpr arg		`thenDs` \ core_arg ->
    dsExprToAtomGivenTy core_arg (coreExprType core_arg)	$ \ atom_arg ->
    returnDs (core_fun `App` atom_arg)

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
dsExpr (OpApp e1 op _ e2)
  = dsExpr op						`thenDs` \ core_op ->
    -- for the type of y, we need the type of op's 2nd argument
    let
	(x_ty:y_ty:_, _) = splitFunTys (coreExprType core_op)
    in
    dsExpr e1				`thenDs` \ x_core ->
    dsExpr e2				`thenDs` \ y_core ->
    dsExprToAtomGivenTy x_core x_ty	$ \ x_atom ->
    dsExprToAtomGivenTy y_core y_ty	$ \ y_atom ->
    returnDs (core_op `App` x_atom `App` y_atom)
    
dsExpr (SectionL expr op)
  = dsExpr op						`thenDs` \ core_op ->
    -- for the type of y, we need the type of op's 2nd argument
    let
	(x_ty:y_ty:_, _) = splitFunTys (coreExprType core_op)
    in
    dsExpr expr				`thenDs` \ x_core ->
    dsExprToAtomGivenTy x_core x_ty	$ \ x_atom ->

    newSysLocalDs y_ty			`thenDs` \ y_id ->
    returnDs (mkValLam [y_id] (core_op `App` x_atom `App` VarArg y_id)) 

-- dsExpr (SectionR op expr)	-- \ x -> op x expr
dsExpr (SectionR op expr)
  = dsExpr op			`thenDs` \ core_op ->
    -- for the type of x, we need the type of op's 2nd argument
    let
	(x_ty:y_ty:_, _) = splitFunTys (coreExprType core_op)
    in
    dsExpr expr				`thenDs` \ y_expr ->
    dsExprToAtomGivenTy y_expr y_ty	$ \ y_atom ->

    newSysLocalDs x_ty			`thenDs` \ x_id ->
    returnDs (mkValLam [x_id] (core_op `App` VarArg x_id `App` y_atom))

dsExpr (CCall label args may_gc is_asm result_ty)
  = mapDs dsExpr args		`thenDs` \ core_args ->
    dsCCall label core_args may_gc is_asm result_ty
	-- dsCCall does all the unboxification, etc.

dsExpr (HsSCC cc expr)
  = dsExpr expr			`thenDs` \ core_expr ->
    getModuleAndGroupDs		`thenDs` \ (mod_name, group_name) ->
    returnDs ( SCC (mkUserCC cc mod_name group_name) core_expr)

dsExpr expr@(HsCase discrim matches src_loc)
  = putSrcLocDs src_loc $
    dsExpr discrim				`thenDs` \ core_discrim ->
    matchWrapper CaseMatch matches "case"	`thenDs` \ ([discrim_var], matching_code) ->
    returnDs ( mkCoLetAny (NonRec discrim_var core_discrim) matching_code )

dsExpr (HsLet binds expr)
  = dsBinds False binds     `thenDs` \ core_binds ->
    dsExpr expr		    `thenDs` \ core_expr ->
    returnDs ( mkCoLetsAny core_binds core_expr )

dsExpr (HsDoOut do_or_lc stmts return_id then_id zero_id result_ty src_loc)
  | maybeToBool maybe_list_comp
  =	-- Special case for list comprehensions
    putSrcLocDs src_loc $
    dsListComp stmts elt_ty

  | otherwise
  = putSrcLocDs src_loc $
    dsDo do_or_lc stmts return_id then_id zero_id result_ty
  where
    maybe_list_comp 
	= case (do_or_lc, splitTyConApp_maybe result_ty) of
	    (ListComp, Just (tycon, [elt_ty]))
		  | tycon == listTyCon
		 -> Just elt_ty
	    other -> Nothing
	-- We need the ListComp form to use deListComp (rather than the "do" form)
	-- because the "return" in a do block is a call to "PrelBase.return", and
	-- not a ReturnStmt.  Only the ListComp form has ReturnStmts

    Just elt_ty = maybe_list_comp

dsExpr (HsIf guard_expr then_expr else_expr src_loc)
  = putSrcLocDs src_loc $
    dsExpr guard_expr	`thenDs` \ core_guard ->
    dsExpr then_expr	`thenDs` \ core_then ->
    dsExpr else_expr	`thenDs` \ core_else ->
    returnDs (mkCoreIfThenElse core_guard core_then core_else)
\end{code}


Type lambda and application
~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
dsExpr (TyLam tyvars expr)
  = dsExpr expr `thenDs` \ core_expr ->
    returnDs (mkTyLam tyvars core_expr)

dsExpr (TyApp expr tys)
  = dsExpr expr		`thenDs` \ core_expr ->
    returnDs (mkTyApp core_expr tys)
\end{code}


Various data construction things
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
dsExpr (ExplicitListOut ty xs)
  = go xs
  where
    list_ty   = mkListTy ty

	-- xs can ocasaionlly be huge, so don't try to take
	-- coreExprType of core_xs, as dsArgToAtom does
	-- (that gives a quadratic algorithm)
    go []     = returnDs (mk_nil_con ty)
    go (x:xs) = dsExpr x				`thenDs` \ core_x ->
		dsExprToAtomGivenTy core_x ty		$ \ arg_x ->
		go xs					`thenDs` \ core_xs ->
		dsExprToAtomGivenTy core_xs list_ty	$ \ arg_xs ->
		returnDs (Con consDataCon [TyArg ty, arg_x, arg_xs])

dsExpr (ExplicitTuple expr_list)
  = mapDs dsExpr expr_list	  `thenDs` \ core_exprs  ->
    mkConDs (tupleCon (length expr_list))
	    (map (TyArg . coreExprType) core_exprs ++ map VarArg core_exprs)

dsExpr (HsCon con_id [ty] [arg])
  | isNewTyCon tycon
  = dsExpr arg		     `thenDs` \ arg' ->
    returnDs (Coerce (CoerceIn con_id) result_ty arg')
  where
    result_ty = mkTyConApp tycon [ty]
    tycon     = dataConTyCon con_id

dsExpr (HsCon con_id tys args)
  = mapDs dsExpr args	 	  `thenDs` \ args2  ->
    mkConDs con_id (map TyArg tys ++ map VarArg args2)

dsExpr (ArithSeqOut expr (From from))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    mkAppDs expr2 [VarArg from2]

dsExpr (ArithSeqOut expr (FromTo from two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr two		  `thenDs` \ two2 ->
    mkAppDs expr2 [VarArg from2, VarArg two2]

dsExpr (ArithSeqOut expr (FromThen from thn))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr thn		  `thenDs` \ thn2 ->
    mkAppDs expr2 [VarArg from2, VarArg thn2]

dsExpr (ArithSeqOut expr (FromThenTo from thn two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr thn		  `thenDs` \ thn2 ->
    dsExpr two		  `thenDs` \ two2 ->
    mkAppDs expr2 [VarArg from2, VarArg thn2, VarArg two2]
\end{code}

Record construction and update
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For record construction we do this (assuming T has three arguments)

	T { op2 = e }
==>
	let err = /\a -> recConErr a 
	T (recConErr t1 "M.lhs/230/op1") 
	  e 
	  (recConErr t1 "M.lhs/230/op3")

recConErr then converts its arugment string into a proper message
before printing it as

	M.lhs, line 230: missing field op1 was evaluated


\begin{code}
dsExpr (RecordCon con_id con_expr rbinds)
  = dsExpr con_expr	`thenDs` \ con_expr' ->
    let
	(arg_tys, _) = splitFunTys (coreExprType con_expr')

	mk_arg (arg_ty, lbl)
	  = case [rhs | (sel_id,rhs,_) <- rbinds,
			lbl == recordSelectorFieldLabel sel_id] of
	      (rhs:rhss) -> ASSERT( null rhss )
		 	    dsExpr rhs
	      []         -> mkErrorAppDs rEC_CON_ERROR_ID arg_ty (showForErr lbl)
    in
    mapDs mk_arg (zipEqual "dsExpr:RecordCon" arg_tys (dataConFieldLabels con_id)) `thenDs` \ con_args ->
    mkAppDs con_expr' (map VarArg con_args)
\end{code}

Record update is a little harder. Suppose we have the decl:

	data T = T1 {op1, op2, op3 :: Int}
	       | T2 {op4, op2 :: Int}
	       | T3

Then we translate as follows:

	r { op2 = e }
===>
	let op2 = e in
	case r of
	  T1 op1 _ op3 -> T1 op1 op2 op3
	  T2 op4 _     -> T2 op4 op2
	  other	       -> recUpdError "M.lhs/230"

It's important that we use the constructor Ids for T1, T2 etc on the
RHSs, and do not generate a Core Con directly, because the constructor
might do some argument-evaluation first; and may have to throw away some
dictionaries.

\begin{code}
dsExpr (RecordUpdOut record_expr record_out_ty dicts rbinds)
  = dsExpr record_expr	 `thenDs` \ record_expr' ->

	-- Desugar the rbinds, and generate let-bindings if
	-- necessary so that we don't lose sharing
    dsRbinds rbinds		$ \ rbinds' ->
    let
	record_in_ty		   = coreExprType record_expr'
	(tycon, in_inst_tys, cons) = splitAlgTyConApp record_in_ty
	(_,     out_inst_tys, _)   = splitAlgTyConApp record_out_ty
	cons_to_upd  	 	   = filter has_all_fields cons

	-- initial_args are passed to every constructor
	initial_args		= map TyArg out_inst_tys ++ map VarArg dicts
		
	mk_val_arg (field, arg_id) 
	  = case [arg | (f, arg) <- rbinds',
			field == recordSelectorFieldLabel f] of
	      (arg:args) -> ASSERT(null args)
			    arg
	      []	 -> VarArg arg_id

	mk_alt con
	  = newSysLocalsDs (dataConArgTys con in_inst_tys)	`thenDs` \ arg_ids ->
	    let 
		val_args = map mk_val_arg (zipEqual "dsExpr:RecordUpd" (dataConFieldLabels con) arg_ids)
	    in
	    returnDs (con, arg_ids, mkGenApp (mkGenApp (Var con) initial_args) val_args)

	mk_default
	  | length cons_to_upd == length cons 
	  = returnDs NoDefault
	  | otherwise			    
	  = newSysLocalDs record_in_ty				`thenDs` \ deflt_id ->
	    mkErrorAppDs rEC_UPD_ERROR_ID record_out_ty ""	`thenDs` \ err ->
	    returnDs (BindDefault deflt_id err)
    in
    mapDs mk_alt cons_to_upd	`thenDs` \ alts ->
    mk_default			`thenDs` \ deflt ->

    returnDs (Case record_expr' (AlgAlts alts deflt))

  where
    has_all_fields :: Id -> Bool
    has_all_fields con_id 
      = all ok rbinds
      where
	con_fields        = dataConFieldLabels con_id
	ok (sel_id, _, _) = recordSelectorFieldLabel sel_id `elem` con_fields
\end{code}

Dictionary lambda and application
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@DictLam@ and @DictApp@ turn into the regular old things.
(OLD:) @DictFunApp@ also becomes a curried application, albeit slightly more
complicated; reminiscent of fully-applied constructors.
\begin{code}
dsExpr (DictLam dictvars expr)
  = dsExpr expr `thenDs` \ core_expr ->
    returnDs (mkValLam dictvars core_expr)

------------------

dsExpr (DictApp expr dicts)	-- becomes a curried application
  = mapDs lookupEnvDs dicts	`thenDs` \ core_dicts ->
    dsExpr expr			`thenDs` \ core_expr ->
    returnDs (foldl (\f d -> f `App` (VarArg d)) core_expr core_dicts)
\end{code}

\begin{code}


#ifdef DEBUG
-- HsSyn constructs that just shouldn't be here:
dsExpr (HsDo _ _ _)	    = panic "dsExpr:HsDo"
dsExpr (ExplicitList _)	    = panic "dsExpr:ExplicitList"
dsExpr (ExprWithTySig _ _)  = panic "dsExpr:ExprWithTySig"
dsExpr (ArithSeqIn _)	    = panic "dsExpr:ArithSeqIn"
#endif

out_of_range_msg			   -- ditto
  = " out of range: [" ++ show minInt ++ ", " ++ show maxInt ++ "]\n"
\end{code}


%--------------------------------------------------------------------

\begin{code}
dsId v
  = lookupEnvDs v	`thenDs` \ v' ->
    returnDs (Var v')
\end{code}

\begin{code}
dsRbinds :: TypecheckedRecordBinds		-- The field bindings supplied
	 -> ([(Id, CoreArg)] -> DsM CoreExpr)	-- A continuation taking the field
	  					-- bindings with atomic rhss
	 -> DsM CoreExpr			-- The result of the continuation,
						-- wrapped in suitable Lets

dsRbinds [] continue_with 
  = continue_with []

dsRbinds ((sel_id, rhs, pun_flag) : rbinds) continue_with
  = dsExpr rhs		 				`thenDs` \ rhs' ->
    dsExprToAtomGivenTy rhs' (coreExprType rhs')	$ \ rhs_atom ->
    dsRbinds rbinds					$ \ rbinds' ->
    continue_with ((sel_id, rhs_atom) : rbinds')
\end{code}	

\begin{code}
-- do_unfold ty_env val_env (Lam (TyBinder tyvar) body) (TyArg ty : args)
--   = do_unfold (addToTyVarEnv ty_env tyvar ty) val_env body args
-- 
-- do_unfold ty_env val_env (Lam (ValBinder binder) body) (arg@(VarArg expr) : args)
--   = dsExprToAtom arg  $ \ arg_atom ->
--     do_unfold ty_env
--      (addOneToIdEnv val_env binder (argToExpr arg_atom))
--	      body args
--
-- do_unfold ty_env val_env body args
--   = 	-- Clone the remaining part of the template
--    uniqSMtoDsM (substCoreExpr val_env ty_env body)	`thenDs` \ body' ->
--
--	-- Apply result to remaining arguments
--    mkAppDs body' args
\end{code}

Basically does the translation given in the Haskell~1.3 report:
\begin{code}
dsDo	:: DoOrListComp
	-> [TypecheckedStmt]
	-> Id		-- id for: return m
	-> Id		-- id for: (>>=) m
	-> Id		-- id for: zero m
	-> Type		-- Element type; the whole expression has type (m t)
	-> DsM CoreExpr

dsDo do_or_lc stmts return_id then_id zero_id result_ty
  = dsId return_id	`thenDs` \ return_ds -> 
    dsId then_id	`thenDs` \ then_ds -> 
    dsId zero_id	`thenDs` \ zero_ds -> 
    let
	(_, b_ty) = splitAppTy result_ty	-- result_ty must be of the form (m b)
	
	go [ReturnStmt expr] 
	  = dsExpr expr			`thenDs` \ expr2 ->
	    mkAppDs return_ds [TyArg b_ty, VarArg expr2]
    
	go (GuardStmt expr locn : stmts)
	  = do_expr expr locn			`thenDs` \ expr2 ->
	    go stmts				`thenDs` \ rest ->
	    mkAppDs zero_ds [TyArg b_ty]	`thenDs` \ zero_expr ->
	    returnDs (mkCoreIfThenElse expr2 rest zero_expr)
    
	go (ExprStmt expr locn : stmts)
	  = do_expr expr locn		`thenDs` \ expr2 ->
	    let
		(_, a_ty) = splitAppTy (coreExprType expr2)	-- Must be of form (m a)
	    in
	    if null stmts then
		returnDs expr2
	    else
		go stmts     		`thenDs` \ rest  ->
		newSysLocalDs a_ty		`thenDs` \ ignored_result_id ->
		mkAppDs then_ds [TyArg a_ty, TyArg b_ty, VarArg expr2, 
				   VarArg (mkValLam [ignored_result_id] rest)]
    
	go (LetStmt binds : stmts )
	  = dsBinds False binds   `thenDs` \ binds2 ->
	    go stmts 		  `thenDs` \ rest   ->
	    returnDs (mkCoLetsAny binds2 rest)
    
	go (BindStmt pat expr locn : stmts)
	  = putSrcLocDs locn $
	    dsExpr expr 	   `thenDs` \ expr2 ->
	    let
		(_, a_ty)  = splitAppTy (coreExprType expr2)	-- Must be of form (m a)
		zero_expr  = TyApp (HsVar zero_id) [b_ty]
		main_match = PatMatch pat (SimpleMatch (
			     HsDoOut do_or_lc stmts return_id then_id zero_id result_ty locn))
		the_matches
		  = if failureFreePat pat
		    then [main_match]
		    else [main_match, PatMatch (WildPat a_ty) (SimpleMatch zero_expr)]
	    in
	    matchWrapper DoBindMatch the_matches match_msg
				`thenDs` \ (binders, matching_code) ->
	    mkAppDs then_ds [TyArg a_ty, TyArg b_ty,
			     VarArg expr2, VarArg (mkValLam binders matching_code)]
    in
    go stmts

  where
    do_expr expr locn = putSrcLocDs locn (dsExpr expr)

    match_msg = case do_or_lc of
			DoStmt   -> "`do' statement"
			ListComp -> "comprehension"
\end{code}
