%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsExpr]{Matching expressions (Exprs)}

\begin{code}
module DsExpr ( dsExpr, dsLet ) where

#include "HsVersions.h"


import HsSyn		( failureFreePat,
			  HsExpr(..), OutPat(..), HsLit(..), ArithSeqInfo(..),
			  Stmt(..), StmtCtxt(..), Match(..), HsBinds(..), MonoBinds(..), 
			  mkSimpleMatch
			)
import TcHsSyn		( TypecheckedHsExpr, TypecheckedHsBinds,
			  TypecheckedStmt,
			  maybeBoxedPrimType

			)
import CoreSyn

import DsMonad
import DsBinds		( dsMonoBinds, AutoScc(..) )
import DsGRHSs		( dsGuarded )
import DsCCall		( dsCCall )
import DsListComp	( dsListComp )
import DsUtils		( mkErrorAppDs )
import Match		( matchWrapper, matchSimply )

import CoreUtils	( coreExprType )
import CostCentre	( mkUserCC )
import FieldLabel	( FieldLabel )
import Id		( Id, idType, recordSelectorFieldLabel )
import Const		( Con(..) )
import DataCon		( DataCon, dataConId, dataConTyCon, dataConArgTys, dataConFieldLabels )
import Const		( mkMachInt, Literal(..), mkStrLit )
import PrelVals		( rEC_CON_ERROR_ID, rEC_UPD_ERROR_ID, iRREFUT_PAT_ERROR_ID )
import TyCon		( isNewTyCon )
import DataCon		( isExistentialDataCon )
import Type		( splitFunTys, mkTyConApp,
			  splitAlgTyConApp, splitTyConApp_maybe, isNotUsgTy, unUsgTy,
			  splitAppTy, isUnLiftedType, Type
			)
import TysWiredIn	( tupleCon, unboxedTupleCon,
			  consDataCon, listTyCon, mkListTy,
			  charDataCon, charTy, stringTy
			)
import BasicTypes	( RecFlag(..) )
import Maybes		( maybeToBool )
import Util		( zipEqual, zipWithEqual )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{dsLet}
%*									*
%************************************************************************

@dsLet@ is a match-result transformer, taking the MatchResult for the body
and transforming it into one for the let-bindings enclosing the body.

This may seem a bit odd, but (source) let bindings can contain unboxed
binds like

	C x# = e

This must be transformed to a case expression and, if the type has
more than one constructor, may fail.

\begin{code}
dsLet :: TypecheckedHsBinds -> CoreExpr -> DsM CoreExpr

dsLet EmptyBinds body
  = returnDs body

dsLet (ThenBinds b1 b2) body
  = dsLet b2 body 	`thenDs` \ body' ->
    dsLet b1 body'
  
-- Special case for bindings which bind unlifted variables
dsLet (MonoBind (AbsBinds [] [] binder_triples (PatMonoBind pat grhss loc)) sigs is_rec) body
  | or [isUnLiftedType (idType g) | (_, g, l) <- binder_triples]
  = ASSERT (case is_rec of {NonRecursive -> True; other -> False})
    putSrcLocDs loc			$
    dsGuarded grhss 			`thenDs` \ rhs ->
    let
	body' = foldr bind body binder_triples
	bind (tyvars, g, l) body = ASSERT( null tyvars )
				   bindNonRec g (Var l) body
    in
    mkErrorAppDs iRREFUT_PAT_ERROR_ID result_ty (showSDoc (ppr pat))	`thenDs` \ error_expr ->
    matchSimply rhs PatBindMatch pat body' error_expr
  where
    result_ty = coreExprType body

-- Ordinary case for bindings
dsLet (MonoBind binds sigs is_rec) body
  = dsMonoBinds NoSccs binds []  `thenDs` \ prs ->
    case is_rec of
      Recursive    -> returnDs (Let (Rec prs) body)
      NonRecursive -> returnDs (foldr mk_let body prs)
  where
    mk_let (bndr,rhs) body = Let (NonRec bndr rhs) body
\end{code}

%************************************************************************
%*									*
\subsection[DsExpr-vars-and-cons]{Variables and constructors}
%*									*
%************************************************************************

\begin{code}
dsExpr :: TypecheckedHsExpr -> DsM CoreExpr

dsExpr e@(HsVar var) = returnDs (Var var)
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
  = returnDs (mkNilExpr charTy)

  | _LENGTH_ s == 1
  = let
	the_char = mkConApp charDataCon [mkLit (MachChar (_HEAD_ s))]
	the_nil  = mkNilExpr charTy
	the_cons = mkConApp consDataCon [Type charTy, the_char, the_nil]
    in
    returnDs the_cons


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
  = returnDs (mkLit (NoRepStr str stringTy))

dsExpr (HsLitOut (HsLitLit str) ty)
  | isUnLiftedType ty
  = returnDs (mkLit (MachLitLit str ty))
  | otherwise
  = case (maybeBoxedPrimType ty) of
      Just (boxing_data_con, prim_ty) ->
	    returnDs ( mkConApp boxing_data_con [mkLit (MachLitLit str prim_ty)] )
      _ -> 
	pprError "ERROR:"
		 (vcat
		   [ hcat [ text "Cannot see data constructor of ``literal-literal''s type: "
		         , text "value:", quotes (quotes (ptext str))
		         , text "; type: ", ppr ty
		         ]
		   , text "Try compiling with -fno-prune-tydecls."
		   ])
		  
  where
    (data_con, prim_ty)
      = case (maybeBoxedPrimType ty) of
	  Just (boxing_data_con, prim_ty) -> (boxing_data_con, prim_ty)
	  Nothing
	    -> pprPanic "ERROR: ``literal-literal'' not a single-constructor type: "
			(hcat [ptext str, text "; type: ", ppr ty])

dsExpr (HsLitOut (HsInt i) ty)
  = returnDs (mkLit (NoRepInteger i ty))

dsExpr (HsLitOut (HsFrac r) ty)
  = returnDs (mkLit (NoRepRational r ty))

-- others where we know what to do:

dsExpr (HsLitOut (HsIntPrim i) _)
  | (i >= toInteger minInt && i <= toInteger maxInt) 
  = returnDs (mkLit (mkMachInt i))
  | otherwise
  = error ("ERROR: Int constant " ++ show i ++ out_of_range_msg)

dsExpr (HsLitOut (HsFloatPrim f) _)
  = returnDs (mkLit (MachFloat f))
    -- ToDo: range checking needed!

dsExpr (HsLitOut (HsDoublePrim d) _)
  = returnDs (mkLit (MachDouble d))
    -- ToDo: range checking needed!

dsExpr (HsLitOut (HsChar c) _)
  = returnDs ( mkConApp charDataCon [mkLit (MachChar c)] )

dsExpr (HsLitOut (HsCharPrim c) _)
  = returnDs (mkLit (MachChar c))

dsExpr (HsLitOut (HsStringPrim s) _)
  = returnDs (mkLit (MachStr s))

-- end of literals magic. --

dsExpr expr@(HsLam a_Match)
  = matchWrapper LambdaMatch [a_Match] "lambda"	`thenDs` \ (binders, matching_code) ->
    returnDs (mkLams binders matching_code)

dsExpr expr@(HsApp fun arg)      
  = dsExpr fun		`thenDs` \ core_fun ->
    dsExpr arg		`thenDs` \ core_arg ->
    returnDs (core_fun `App` core_arg)

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
    returnDs (mkApps core_op [x_core, y_core])
    
dsExpr (SectionL expr op)
  = dsExpr op						`thenDs` \ core_op ->
    -- for the type of y, we need the type of op's 2nd argument
    let
	(x_ty:y_ty:_, _) = splitFunTys (coreExprType core_op)
    in
    dsExpr expr				`thenDs` \ x_core ->
    newSysLocalDs x_ty			`thenDs` \ x_id ->
    newSysLocalDs y_ty			`thenDs` \ y_id ->

    returnDs (bindNonRec x_id x_core $
	      Lam y_id (mkApps core_op [Var x_id, Var y_id]))

-- dsExpr (SectionR op expr)	-- \ x -> op x expr
dsExpr (SectionR op expr)
  = dsExpr op			`thenDs` \ core_op ->
    -- for the type of x, we need the type of op's 2nd argument
    let
	(x_ty:y_ty:_, _) = splitFunTys (coreExprType core_op)
    in
    dsExpr expr				`thenDs` \ y_core ->
    newSysLocalDs x_ty			`thenDs` \ x_id ->
    newSysLocalDs y_ty			`thenDs` \ y_id ->

    returnDs (bindNonRec y_id y_core $
	      Lam x_id (mkApps core_op [Var x_id, Var y_id]))

dsExpr (CCall label args may_gc is_asm result_ty)
  = mapDs dsExpr args		`thenDs` \ core_args ->
    dsCCall label core_args may_gc is_asm result_ty
	-- dsCCall does all the unboxification, etc.

dsExpr (HsSCC cc expr)
  = dsExpr expr			`thenDs` \ core_expr ->
    getModuleAndGroupDs		`thenDs` \ (mod_name, group_name) ->
    returnDs (Note (SCC (mkUserCC cc mod_name group_name)) core_expr)

-- special case to handle unboxed tuple patterns.

dsExpr (HsCase discrim matches@[Match _ [TuplePat ps boxed] _ _] src_loc)
 | not boxed && all var_pat ps 
 =  putSrcLocDs src_loc $
    dsExpr discrim				`thenDs` \ core_discrim ->
    matchWrapper CaseMatch matches "case"	`thenDs` \ ([discrim_var], matching_code) ->
    case matching_code of
	Case (Var x) bndr alts | x == discrim_var -> 
		returnDs (Case core_discrim bndr alts)
	_ -> panic ("dsExpr: tuple pattern:\n" ++ showSDoc (ppr matching_code))

dsExpr (HsCase discrim matches src_loc)
  = putSrcLocDs src_loc $
    dsExpr discrim				`thenDs` \ core_discrim ->
    matchWrapper CaseMatch matches "case"	`thenDs` \ ([discrim_var], matching_code) ->
    returnDs (bindNonRec discrim_var core_discrim matching_code)

dsExpr (HsLet binds body)
  = dsExpr body		`thenDs` \ body' ->
    dsLet binds body'
    
dsExpr (HsDoOut do_or_lc stmts return_id then_id fail_id result_ty src_loc)
  | maybeToBool maybe_list_comp
  =	-- Special case for list comprehensions
    putSrcLocDs src_loc $
    dsListComp stmts elt_ty

  | otherwise
  = putSrcLocDs src_loc $
    dsDo do_or_lc stmts return_id then_id fail_id result_ty
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
    returnDs (mkIfThenElse core_guard core_then core_else)
\end{code}


Type lambda and application
~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
dsExpr (TyLam tyvars expr)
  = dsExpr expr `thenDs` \ core_expr ->
    returnDs (mkLams tyvars core_expr)

dsExpr (TyApp expr tys)
  = dsExpr expr		`thenDs` \ core_expr ->
    returnDs (mkTyApps core_expr tys)
\end{code}


Various data construction things
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
dsExpr (ExplicitListOut ty xs)
  = go xs
  where
    list_ty   = mkListTy ty

    go []     = returnDs (mkNilExpr ty)
    go (x:xs) = dsExpr x				`thenDs` \ core_x ->
		go xs					`thenDs` \ core_xs ->
                ASSERT( isNotUsgTy ty )
		returnDs (mkConApp consDataCon [Type ty, core_x, core_xs])

dsExpr (ExplicitTuple expr_list boxed)
  = mapDs dsExpr expr_list	  `thenDs` \ core_exprs  ->
    returnDs (mkConApp ((if boxed 
			    then tupleCon 
			    else unboxedTupleCon) (length expr_list))
	    	(map (Type . unUsgTy . coreExprType) core_exprs ++ core_exprs))
                -- the above unUsgTy is *required* -- KSW 1999-04-07

dsExpr (HsCon con_id [ty] [arg])
  | isNewTyCon tycon
  = dsExpr arg		     `thenDs` \ arg' ->
    returnDs (Note (Coerce result_ty (unUsgTy (coreExprType arg'))) arg')
  where
    result_ty = mkTyConApp tycon [ty]
    tycon     = dataConTyCon con_id

dsExpr (HsCon con_id tys args)
  = mapDs dsExpr args	 	  `thenDs` \ args2  ->
    ASSERT( all isNotUsgTy tys )
    returnDs (mkConApp con_id (map Type tys ++ args2))

dsExpr (ArithSeqOut expr (From from))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    returnDs (App expr2 from2)

dsExpr (ArithSeqOut expr (FromTo from two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr two		  `thenDs` \ two2 ->
    returnDs (mkApps expr2 [from2, two2])

dsExpr (ArithSeqOut expr (FromThen from thn))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr thn		  `thenDs` \ thn2 ->
    returnDs (mkApps expr2 [from2, thn2])

dsExpr (ArithSeqOut expr (FromThenTo from thn two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr thn		  `thenDs` \ thn2 ->
    dsExpr two		  `thenDs` \ two2 ->
    returnDs (mkApps expr2 [from2, thn2, two2])
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
dsExpr (RecordConOut data_con con_expr rbinds)
  = dsExpr con_expr	`thenDs` \ con_expr' ->
    let
	(arg_tys, _) = splitFunTys (coreExprType con_expr')

	mk_arg (arg_ty, lbl)
	  = case [rhs | (sel_id,rhs,_) <- rbinds,
			lbl == recordSelectorFieldLabel sel_id] of
	      (rhs:rhss) -> ASSERT( null rhss )
		 	    dsExpr rhs
	      []         -> mkErrorAppDs rEC_CON_ERROR_ID arg_ty (showSDoc (ppr lbl))
    in
    mapDs mk_arg (zipEqual "dsExpr:RecordCon" arg_tys (dataConFieldLabels data_con)) `thenDs` \ con_args ->
    returnDs (mkApps con_expr' con_args)
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
  = dsExpr record_expr	 	`thenDs` \ record_expr' ->

	-- Desugar the rbinds, and generate let-bindings if
	-- necessary so that we don't lose sharing

    let
	ds_rbind (sel_id, rhs, pun_flag)
	  = dsExpr rhs		 		`thenDs` \ rhs' ->
	    returnDs (recordSelectorFieldLabel sel_id, rhs')
    in
    mapDs ds_rbind rbinds			`thenDs` \ rbinds' ->
    let
	record_in_ty		   = coreExprType record_expr'
	(tycon, in_inst_tys, cons) = splitAlgTyConApp record_in_ty
	(_,     out_inst_tys, _)   = splitAlgTyConApp record_out_ty
	cons_to_upd  	 	   = filter has_all_fields cons

	-- initial_args are passed to every constructor
	initial_args		= map Type out_inst_tys ++ map Var dicts
		
	mk_val_arg field old_arg_id 
	  = case [rhs | (f, rhs) <- rbinds', field == f] of
	      (rhs:rest) -> ASSERT(null rest) rhs
	      []	 -> Var old_arg_id

	mk_alt con
	  = newSysLocalsDs (dataConArgTys con in_inst_tys)	`thenDs` \ arg_ids ->
	    let 
		val_args = zipWithEqual "dsExpr:RecordUpd" mk_val_arg
					(dataConFieldLabels con) arg_ids
		rhs = mkApps (mkApps (Var (dataConId con)) initial_args) val_args
	    in
	    returnDs (DataCon con, arg_ids, rhs)

	mk_default
	  | length cons_to_upd == length cons 
	  = returnDs []
	  | otherwise			    
	  = mkErrorAppDs rEC_UPD_ERROR_ID record_out_ty ""	`thenDs` \ err ->
	    returnDs [(DEFAULT, [], err)]
    in
	-- Record stuff doesn't work for existentials
    ASSERT( all (not . isExistentialDataCon) cons )

    newSysLocalDs record_in_ty	`thenDs` \ case_bndr ->
    mapDs mk_alt cons_to_upd	`thenDs` \ alts ->
    mk_default			`thenDs` \ deflt ->

    returnDs (Case record_expr' case_bndr (alts ++ deflt))
  where
    has_all_fields :: DataCon -> Bool
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
    returnDs (mkLams dictvars core_expr)

------------------

dsExpr (DictApp expr dicts)	-- becomes a curried application
  = dsExpr expr			`thenDs` \ core_expr ->
    returnDs (foldl (\f d -> f `App` (Var d)) core_expr dicts)
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

Basically does the translation given in the Haskell~1.3 report:

\begin{code}
dsDo	:: StmtCtxt
	-> [TypecheckedStmt]
	-> Id		-- id for: return m
	-> Id		-- id for: (>>=) m
	-> Id		-- id for: fail m
	-> Type		-- Element type; the whole expression has type (m t)
	-> DsM CoreExpr

dsDo do_or_lc stmts return_id then_id fail_id result_ty
  = let
	(_, b_ty) = splitAppTy result_ty	-- result_ty must be of the form (m b)
	
	go [ReturnStmt expr] 
	  = dsExpr expr			`thenDs` \ expr2 ->
	    returnDs (mkApps (Var return_id) [Type b_ty, expr2])
    
	go (GuardStmt expr locn : stmts)
	  = do_expr expr locn			`thenDs` \ expr2 ->
	    go stmts				`thenDs` \ rest ->
	    let msg = ASSERT( isNotUsgTy b_ty )
                      "Pattern match failure in do expression, " ++ showSDoc (ppr locn) in
	    returnDs (mkIfThenElse expr2 
				   rest 
				   (App (App (Var fail_id) 
					     (Type b_ty))
					     (mkLit (mkStrLit msg stringTy))))
    
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
		returnDs (mkApps (Var then_id) [Type a_ty, Type b_ty, expr2, 
					        Lam ignored_result_id rest])
    
	go (LetStmt binds : stmts )
	  = go stmts 		`thenDs` \ rest   ->
	    dsLet binds	rest
	    
	go (BindStmt pat expr locn : stmts)
	  = putSrcLocDs locn $
	    dsExpr expr 	   `thenDs` \ expr2 ->
	    let
		(_, a_ty)  = splitAppTy (coreExprType expr2)	-- Must be of form (m a)
		fail_expr  = HsApp (TyApp (HsVar fail_id) [b_ty]) (HsLitOut (HsString (_PK_ msg)) stringTy)
	        msg = ASSERT2( isNotUsgTy a_ty, ppr a_ty )
                      ASSERT2( isNotUsgTy b_ty, ppr b_ty )
                      "Pattern match failure in do expression, " ++ showSDoc (ppr locn)
		main_match = mkSimpleMatch [pat] 
					   (HsDoOut do_or_lc stmts return_id then_id fail_id result_ty locn)
					   (Just result_ty) locn
		the_matches
		  | failureFreePat pat = [main_match]
		  | otherwise	       =
		      [ main_match
		      , mkSimpleMatch [WildPat a_ty] fail_expr (Just result_ty) locn
		      ]
	    in
	    matchWrapper DoBindMatch the_matches match_msg
				`thenDs` \ (binders, matching_code) ->
	    returnDs (mkApps (Var then_id) [Type a_ty, Type b_ty, expr2,
				            mkLams binders matching_code])
    in
    go stmts

  where
    do_expr expr locn = putSrcLocDs locn (dsExpr expr)

    match_msg = case do_or_lc of
			DoStmt   -> "`do' statement"
			ListComp -> "comprehension"
\end{code}

\begin{code}
var_pat (WildPat _) = True
var_pat (VarPat _) = True
var_pat _ = False
\end{code}

