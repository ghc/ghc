%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsExpr]{Matching expressions (Exprs)}

\begin{code}
module DsExpr ( dsExpr, dsLet ) where

#include "HsVersions.h"


import HsSyn		( failureFreePat,
			  HsExpr(..), OutPat(..), HsLit(..), ArithSeqInfo(..),
			  Stmt(..), HsMatchContext(..), HsDoContext(..), 
			  Match(..), HsBinds(..), MonoBinds(..), 
			  mkSimpleMatch 
			)
import TcHsSyn		( TypecheckedHsExpr, TypecheckedHsBinds, TypecheckedStmt, outPatType )

-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types (newtypes etc), and sometimes not
--     So WATCH OUT; check each use of split*Ty functions.
-- Sigh.  This is a pain.

import TcType		( tcSplitAppTy, tcSplitFunTys, tcSplitTyConApp_maybe, tcTyConAppArgs,
			  isIntegerTy, tcSplitTyConApp, isUnLiftedType, Type )
import Type		( splitFunTys )
import CoreSyn
import CoreUtils	( exprType, mkIfThenElse, bindNonRec )

import DsMonad
import DsBinds		( dsMonoBinds, AutoScc(..) )
import DsGRHSs		( dsGuarded )
import DsCCall		( dsCCall, resultWrapper )
import DsListComp	( dsListComp )
import DsUtils		( mkErrorAppDs, mkStringLit, mkStringLitFS, 
			  mkConsExpr, mkNilExpr, mkIntegerLit
			)
import Match		( matchWrapper, matchSimply )

import FieldLabel	( FieldLabel, fieldLabelTyCon )
import CostCentre	( mkUserCC )
import Id		( Id, idType, recordSelectorFieldLabel )
import PrelInfo		( rEC_CON_ERROR_ID, iRREFUT_PAT_ERROR_ID )
import DataCon		( DataCon, dataConWrapId, dataConFieldLabels, dataConInstOrigArgTys )
import DataCon		( isExistentialDataCon )
import Literal		( Literal(..) )
import TyCon		( tyConDataCons )
import TysWiredIn	( tupleCon, listTyCon, charDataCon, intDataCon )
import BasicTypes	( RecFlag(..), Boxity(..) )
import Maybes		( maybeToBool )
import PrelNames	( hasKey, ratioTyConKey )
import Util		( zipEqual, zipWithEqual )
import Outputable

import Ratio 		( numerator, denominator )
\end{code}


%************************************************************************
%*									*
\subsection{dsLet}
%*									*
%************************************************************************

@dsLet@ is a match-result transformer, taking the @MatchResult@ for the body
and transforming it into one for the let-bindings enclosing the body.

This may seem a bit odd, but (source) let bindings can contain unboxed
binds like
\begin{verbatim}
	C x# = e
\end{verbatim}
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
-- We need to do a case right away, rather than building
-- a tuple and doing selections.
-- Silently ignore INLINE pragmas...
dsLet (MonoBind (AbsBinds [] [] exports inlines binds) sigs is_rec) body
  | or [isUnLiftedType (idType g) | (_, g, l) <- exports]
  = ASSERT (case is_rec of {NonRecursive -> True; other -> False})
	-- Unlifted bindings are always non-recursive
	-- and are always a Fun or Pat monobind
	--
	-- ToDo: in some bizarre case it's conceivable that there
	--       could be dict binds in the 'binds'.  (See the notes
	--	 below.  Then pattern-match would fail.  Urk.)
    case binds of
      FunMonoBind fun _ matches loc
	-> putSrcLocDs loc			$
	   matchWrapper (FunRhs fun) matches 	`thenDs` \ (args, rhs) ->
	   ASSERT( null args )	-- Functions aren't lifted
	   returnDs (bindNonRec fun rhs body_w_exports)

      PatMonoBind pat grhss loc
	-> putSrcLocDs loc			$
	   dsGuarded grhss 			`thenDs` \ rhs ->
	   mk_error_app pat			`thenDs` \ error_expr ->
	   matchSimply rhs PatBindRhs pat body_w_exports error_expr
  where
    body_w_exports		 = foldr bind_export body exports
    bind_export (tvs, g, l) body = ASSERT( null tvs )
				   bindNonRec g (Var l) body

    mk_error_app pat = mkErrorAppDs iRREFUT_PAT_ERROR_ID
				    (exprType body)
				    (showSDoc (ppr pat))

-- Ordinary case for bindings
dsLet (MonoBind binds sigs is_rec) body
  = dsMonoBinds NoSccs binds []  `thenDs` \ prs ->
    returnDs (Let (Rec prs) body)
	-- Use a Rec regardless of is_rec. 
	-- Why? Because it allows the MonoBinds to be all
	-- mixed up, which is what happens in one rare case
	-- Namely, for an AbsBind with no tyvars and no dicts,
	-- 	   but which does have dictionary bindings.
	-- See notes with TcSimplify.inferLoop [NO TYVARS]
	-- It turned out that wrapping a Rec here was the easiest solution
	--
	-- NB The previous case dealt with unlifted bindings, so we
	--    only have to deal with lifted ones now; so Rec is ok
\end{code}	

%************************************************************************
%*									*
\subsection[DsExpr-vars-and-cons]{Variables, constructors, literals}
%*									*
%************************************************************************

\begin{code}
dsExpr :: TypecheckedHsExpr -> DsM CoreExpr

dsExpr (HsVar var)	 = returnDs (Var var)
dsExpr (HsIPVar var)     = returnDs (Var var)
dsExpr (HsLit lit)       = dsLit lit
-- HsOverLit has been gotten rid of by the type checker

dsExpr expr@(HsLam a_Match)
  = matchWrapper LambdaExpr [a_Match]	`thenDs` \ (binders, matching_code) ->
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
    dsExpr e1				`thenDs` \ x_core ->
    dsExpr e2				`thenDs` \ y_core ->
    returnDs (mkApps core_op [x_core, y_core])
    
dsExpr (SectionL expr op)
  = dsExpr op						`thenDs` \ core_op ->
    -- for the type of y, we need the type of op's 2nd argument
    let
	(x_ty:y_ty:_, _) = splitFunTys (exprType core_op)
	-- Must look through an implicit-parameter type; 
	-- newtype impossible; hence Type.splitFunTys
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
	(x_ty:y_ty:_, _) = splitFunTys (exprType core_op)
	-- See comment with SectionL
    in
    dsExpr expr				`thenDs` \ y_core ->
    newSysLocalDs x_ty			`thenDs` \ x_id ->
    newSysLocalDs y_ty			`thenDs` \ y_id ->

    returnDs (bindNonRec y_id y_core $
	      Lam x_id (mkApps core_op [Var x_id, Var y_id]))

dsExpr (HsCCall lbl args may_gc is_asm result_ty)
  = mapDs dsExpr args		`thenDs` \ core_args ->
    dsCCall lbl core_args may_gc is_asm result_ty
	-- dsCCall does all the unboxification, etc.

dsExpr (HsSCC cc expr)
  = dsExpr expr			`thenDs` \ core_expr ->
    getModuleDs			`thenDs` \ mod_name ->
    returnDs (Note (SCC (mkUserCC cc mod_name)) core_expr)

-- special case to handle unboxed tuple patterns.

dsExpr (HsCase discrim matches src_loc)
 | all ubx_tuple_match matches
 =  putSrcLocDs src_loc $
    dsExpr discrim			`thenDs` \ core_discrim ->
    matchWrapper CaseAlt matches 	`thenDs` \ ([discrim_var], matching_code) ->
    case matching_code of
	Case (Var x) bndr alts | x == discrim_var -> 
		returnDs (Case core_discrim bndr alts)
	_ -> panic ("dsExpr: tuple pattern:\n" ++ showSDoc (ppr matching_code))
  where
    ubx_tuple_match (Match [TuplePat ps Unboxed] _ _) = True
    ubx_tuple_match _ = False

dsExpr (HsCase discrim matches src_loc)
  = putSrcLocDs src_loc $
    dsExpr discrim			`thenDs` \ core_discrim ->
    matchWrapper CaseAlt matches	`thenDs` \ ([discrim_var], matching_code) ->
    returnDs (bindNonRec discrim_var core_discrim matching_code)

dsExpr (HsLet binds body)
  = dsExpr body		`thenDs` \ body' ->
    dsLet binds body'

dsExpr (HsWith expr binds)
  = dsExpr expr		`thenDs` \ expr' ->
    foldlDs dsIPBind expr' binds
    where
      dsIPBind body (n, e)
        = dsExpr e	`thenDs` \ e' ->
	  returnDs (Let (NonRec n e') body)

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
	= case (do_or_lc, tcSplitTyConApp_maybe result_ty) of
	    (ListComp, Just (tycon, [elt_ty]))
		  | tycon == listTyCon
		 -> Just elt_ty
	    other -> Nothing
	-- We need the ListComp form to use deListComp (rather than the "do" form)
	-- because the interpretation of ExprStmt depends on what sort of thing
	-- it is.

    Just elt_ty = maybe_list_comp

dsExpr (HsIf guard_expr then_expr else_expr src_loc)
  = putSrcLocDs src_loc $
    dsExpr guard_expr	`thenDs` \ core_guard ->
    dsExpr then_expr	`thenDs` \ core_then ->
    dsExpr else_expr	`thenDs` \ core_else ->
    returnDs (mkIfThenElse core_guard core_then core_else)
\end{code}


\noindent
\underline{\bf Type lambda and application}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
dsExpr (TyLam tyvars expr)
  = dsExpr expr `thenDs` \ core_expr ->
    returnDs (mkLams tyvars core_expr)

dsExpr (TyApp expr tys)
  = dsExpr expr		`thenDs` \ core_expr ->
    returnDs (mkTyApps core_expr tys)
\end{code}


\noindent
\underline{\bf Various data construction things}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
dsExpr (ExplicitList ty xs)
  = go xs
  where
    go []     = returnDs (mkNilExpr ty)
    go (x:xs) = dsExpr x				`thenDs` \ core_x ->
		go xs					`thenDs` \ core_xs ->
		returnDs (mkConsExpr ty core_x core_xs)

dsExpr (ExplicitTuple expr_list boxity)
  = mapDs dsExpr expr_list	  `thenDs` \ core_exprs  ->
    returnDs (mkConApp (tupleCon boxity (length expr_list))
	    	       (map (Type .  exprType) core_exprs ++ core_exprs))

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

\noindent
\underline{\bf Record construction and update}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For record construction we do this (assuming T has three arguments)
\begin{verbatim}
	T { op2 = e }
==>
	let err = /\a -> recConErr a 
	T (recConErr t1 "M.lhs/230/op1") 
	  e 
	  (recConErr t1 "M.lhs/230/op3")
\end{verbatim}
@recConErr@ then converts its arugment string into a proper message
before printing it as
\begin{verbatim}
	M.lhs, line 230: missing field op1 was evaluated
\end{verbatim}

We also handle @C{}@ as valid construction syntax for an unlabelled
constructor @C@, setting all of @C@'s fields to bottom.

\begin{code}
dsExpr (RecordConOut data_con con_expr rbinds)
  = dsExpr con_expr	`thenDs` \ con_expr' ->
    let
	(arg_tys, _) = tcSplitFunTys (exprType con_expr')
	-- A newtype in the corner should be opaque; 
	-- hence TcType.tcSplitFunTys

	mk_arg (arg_ty, lbl)
	  = case [rhs | (sel_id,rhs,_) <- rbinds,
			lbl == recordSelectorFieldLabel sel_id] of
	      (rhs:rhss) -> ASSERT( null rhss )
		 	    dsExpr rhs
	      []         -> mkErrorAppDs rEC_CON_ERROR_ID arg_ty (showSDoc (ppr lbl))
	unlabelled_bottom arg_ty = mkErrorAppDs rEC_CON_ERROR_ID arg_ty ""

	labels = dataConFieldLabels data_con
    in

    (if null labels
	then mapDs unlabelled_bottom arg_tys
	else mapDs mk_arg (zipEqual "dsExpr:RecordCon" arg_tys labels))
	`thenDs` \ con_args ->

    returnDs (mkApps con_expr' con_args)
\end{code}

Record update is a little harder. Suppose we have the decl:
\begin{verbatim}
	data T = T1 {op1, op2, op3 :: Int}
	       | T2 {op4, op2 :: Int}
	       | T3
\end{verbatim}
Then we translate as follows:
\begin{verbatim}
	r { op2 = e }
===>
	let op2 = e in
	case r of
	  T1 op1 _ op3 -> T1 op1 op2 op3
	  T2 op4 _     -> T2 op4 op2
	  other	       -> recUpdError "M.lhs/230"
\end{verbatim}
It's important that we use the constructor Ids for @T1@, @T2@ etc on the
RHSs, and do not generate a Core constructor application directly, because the constructor
might do some argument-evaluation first; and may have to throw away some
dictionaries.

\begin{code}
dsExpr (RecordUpdOut record_expr record_in_ty record_out_ty dicts [])
  = dsExpr record_expr

dsExpr (RecordUpdOut record_expr record_in_ty record_out_ty dicts rbinds)
  = getSrcLocDs			`thenDs` \ src_loc ->
    dsExpr record_expr	 	`thenDs` \ record_expr' ->

	-- Desugar the rbinds, and generate let-bindings if
	-- necessary so that we don't lose sharing

    let
	in_inst_tys  = tcTyConAppArgs record_in_ty	-- Newtype opaque
	out_inst_tys = tcTyConAppArgs record_out_ty	-- Newtype opaque

	mk_val_arg field old_arg_id 
	  = case [rhs | (sel_id, rhs, _) <- rbinds, 
			field == recordSelectorFieldLabel sel_id] of
	      (rhs:rest) -> ASSERT(null rest) rhs
	      []	 -> HsVar old_arg_id

	mk_alt con
	  = newSysLocalsDs (dataConInstOrigArgTys con in_inst_tys) `thenDs` \ arg_ids ->
		-- This call to dataConArgTys won't work for existentials
	    let 
		val_args = zipWithEqual "dsExpr:RecordUpd" mk_val_arg
					(dataConFieldLabels con) arg_ids
		rhs = foldl HsApp (DictApp (TyApp (HsVar (dataConWrapId con)) 
						  out_inst_tys)
					   dicts)
				  val_args
	    in
	    returnDs (mkSimpleMatch [ConPat con record_in_ty [] [] (map VarPat arg_ids)]
				    rhs
				    record_out_ty
				    src_loc)
    in
	-- Record stuff doesn't work for existentials
    ASSERT( all (not . isExistentialDataCon) data_cons )

	-- It's important to generate the match with matchWrapper,
	-- and the right hand sides with applications of the wrapper Id
	-- so that everything works when we are doing fancy unboxing on the
	-- constructor aguments.
    mapDs mk_alt cons_to_upd		`thenDs` \ alts ->
    matchWrapper RecUpd alts		`thenDs` \ ([discrim_var], matching_code) ->

    returnDs (bindNonRec discrim_var record_expr' matching_code)

  where
    updated_fields :: [FieldLabel]
    updated_fields = [recordSelectorFieldLabel sel_id | (sel_id,_,_) <- rbinds]

	-- Get the type constructor from the first field label, 
	-- so that we are sure it'll have all its DataCons
	-- (In GHCI, it's possible that some TyCons may not have all
	--  their constructors, in a module-loop situation.)
    tycon       = fieldLabelTyCon (head updated_fields)
    data_cons   = tyConDataCons tycon
    cons_to_upd = filter has_all_fields data_cons

    has_all_fields :: DataCon -> Bool
    has_all_fields con_id 
      = all (`elem` con_fields) updated_fields
      where
	con_fields = dataConFieldLabels con_id
\end{code}


\noindent
\underline{\bf Dictionary lambda and application}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
dsExpr (ExprWithTySig _ _)  = panic "dsExpr:ExprWithTySig"
dsExpr (ArithSeqIn _)	    = panic "dsExpr:ArithSeqIn"
#endif

\end{code}

%--------------------------------------------------------------------

Basically does the translation given in the Haskell~1.3 report:

\begin{code}
dsDo	:: HsDoContext
	-> [TypecheckedStmt]
	-> Id		-- id for: return m
	-> Id		-- id for: (>>=) m
	-> Id		-- id for: fail m
	-> Type		-- Element type; the whole expression has type (m t)
	-> DsM CoreExpr

dsDo do_or_lc stmts return_id then_id fail_id result_ty
  = let
	(_, b_ty) = tcSplitAppTy result_ty	-- result_ty must be of the form (m b)
	is_do	  = case do_or_lc of
			DoExpr   -> True
			ListComp -> False
	
	-- For ExprStmt, see the comments near HsExpr.Stmt about 
	-- exactly what ExprStmts mean!
	--
	-- In dsDo we can only see DoStmt and ListComp (no gaurds)

	go [ResultStmt expr locn]
	  | is_do     = do_expr expr locn
	  | otherwise = do_expr expr locn	`thenDs` \ expr2 ->
			returnDs (mkApps (Var return_id) [Type b_ty, expr2])

	go (ExprStmt expr a_ty locn : stmts)
	  | is_do	-- Do expression
	  = do_expr expr locn		`thenDs` \ expr2 ->
	    go stmts     		`thenDs` \ rest  ->
	    newSysLocalDs a_ty		`thenDs` \ ignored_result_id ->
	    returnDs (mkApps (Var then_id) [Type a_ty, Type b_ty, expr2, 
					    Lam ignored_result_id rest])

	   | otherwise	-- List comprehension
	  = do_expr expr locn			`thenDs` \ expr2 ->
	    go stmts				`thenDs` \ rest ->
	    let
		msg = "Pattern match failure in do expression, " ++ showSDoc (ppr locn)
	    in
	    mkStringLit msg			`thenDs` \ core_msg ->
	    returnDs (mkIfThenElse expr2 rest 
				   (App (App (Var fail_id) (Type b_ty)) core_msg))
    
	go (LetStmt binds : stmts )
	  = go stmts 		`thenDs` \ rest   ->
	    dsLet binds	rest
	    
	go (BindStmt pat expr locn : stmts)
	  = putSrcLocDs locn $
	    dsExpr expr 	   `thenDs` \ expr2 ->
	    let
		a_ty       = outPatType pat
		fail_expr  = HsApp (TyApp (HsVar fail_id) [b_ty])
                                   (HsLit (HsString (_PK_ msg)))
	        msg = "Pattern match failure in do expression, " ++ showSDoc (ppr locn)
		main_match = mkSimpleMatch [pat] 
					   (HsDoOut do_or_lc stmts return_id then_id
                                                    fail_id result_ty locn)
					   result_ty locn
		the_matches
		  | failureFreePat pat = [main_match]
		  | otherwise	       =
		      [ main_match
		      , mkSimpleMatch [WildPat a_ty] fail_expr result_ty locn
		      ]
	    in
	    matchWrapper (DoCtxt do_or_lc) the_matches	`thenDs` \ (binders, matching_code) ->
	    returnDs (mkApps (Var then_id) [Type a_ty, Type b_ty, expr2,
				            mkLams binders matching_code])
    in
    go stmts

  where
    do_expr expr locn = putSrcLocDs locn (dsExpr expr)
\end{code}


%************************************************************************
%*									*
\subsection[DsExpr-literals]{Literals}
%*									*
%************************************************************************

We give int/float literals type @Integer@ and @Rational@, respectively.
The typechecker will (presumably) have put \tr{from{Integer,Rational}s}
around them.

ToDo: put in range checks for when converting ``@i@''
(or should that be in the typechecker?)

For numeric literals, we try to detect there use at a standard type
(@Int@, @Float@, etc.) are directly put in the right constructor.
[NB: down with the @App@ conversion.]

See also below where we look for @DictApps@ for \tr{plusInt}, etc.

\begin{code}
dsLit :: HsLit -> DsM CoreExpr
dsLit (HsChar c)       = returnDs (mkConApp charDataCon [mkLit (MachChar c)])
dsLit (HsCharPrim c)   = returnDs (mkLit (MachChar c))
dsLit (HsString str)   = mkStringLitFS str
dsLit (HsStringPrim s) = returnDs (mkLit (MachStr s))
dsLit (HsInteger i)    = mkIntegerLit i
dsLit (HsInt i)	       = returnDs (mkConApp intDataCon [mkIntLit i])
dsLit (HsIntPrim i)    = returnDs (mkIntLit i)
dsLit (HsFloatPrim f)  = returnDs (mkLit (MachFloat f))
dsLit (HsDoublePrim d) = returnDs (mkLit (MachDouble d))
dsLit (HsLitLit str ty)
  = ASSERT( maybeToBool maybe_ty )
    returnDs (wrap_fn (mkLit (MachLitLit str rep_ty)))
  where
    (maybe_ty, wrap_fn) = resultWrapper ty
    Just rep_ty 	= maybe_ty

dsLit (HsRat r ty)
  = mkIntegerLit (numerator r)		`thenDs` \ num ->
    mkIntegerLit (denominator r)	`thenDs` \ denom ->
    returnDs (mkConApp ratio_data_con [Type integer_ty, num, denom])
  where
    (ratio_data_con, integer_ty) 
	= case tcSplitTyConApp ty of
		(tycon, [i_ty]) -> ASSERT(isIntegerTy i_ty && tycon `hasKey` ratioTyConKey)
				   (head (tyConDataCons tycon), i_ty)
\end{code}
