%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsExpr]{Matching expressions (Exprs)}

\begin{code}
module DsExpr ( dsExpr, dsLet, dsLit ) where

#include "HsVersions.h"


import Match		( matchWrapper, matchSimply )
import MatchLit		( dsLit )
import DsBinds		( dsMonoBinds, AutoScc(..) )
import DsGRHSs		( dsGuarded )
import DsCCall		( dsCCall )
import DsListComp	( dsListComp, dsPArrComp )
import DsUtils		( mkErrorAppDs, mkStringLit, mkConsExpr, mkNilExpr,
			  mkCoreTupTy, selectMatchVar,
			  dsReboundNames, lookupReboundName )
import DsArrows		( dsProcExpr )
import DsMonad

#ifdef GHCI
	-- Template Haskell stuff iff bootstrapped
import DsMeta		( dsBracket, dsReify )
#endif

import HsSyn		( HsExpr(..), Pat(..), ArithSeqInfo(..),
			  Stmt(..), HsMatchContext(..), HsStmtContext(..), 
			  Match(..), HsBinds(..), MonoBinds(..), HsConDetails(..),
			  ReboundNames,
			  mkSimpleMatch, isDoExpr
			)
import TcHsSyn		( TypecheckedHsExpr, TypecheckedHsBinds, TypecheckedStmt, hsPatType )

-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types (newtypes etc), and sometimes not
--     So WATCH OUT; check each use of split*Ty functions.
-- Sigh.  This is a pain.

import TcType		( tcSplitAppTy, tcSplitFunTys, tcTyConAppArgs,
			  tcSplitTyConApp, isUnLiftedType, Type,
			  mkAppTy )
import Type		( splitFunTys )
import CoreSyn
import CoreUtils	( exprType, mkIfThenElse, bindNonRec )

import FieldLabel	( FieldLabel, fieldLabelTyCon )
import CostCentre	( mkUserCC )
import Id		( Id, idType, idName, recordSelectorFieldLabel )
import PrelInfo		( rEC_CON_ERROR_ID, iRREFUT_PAT_ERROR_ID )
import DataCon		( DataCon, dataConWrapId, dataConFieldLabels, dataConInstOrigArgTys )
import DataCon		( isExistentialDataCon )
import Name		( Name )
import TyCon		( tyConDataCons )
import TysWiredIn	( tupleCon )
import BasicTypes	( RecFlag(..), Boxity(..), ipNameName )
import PrelNames	( toPName,
			  returnMName, bindMName, thenMName, failMName,
			  mfixName )
import SrcLoc		( noSrcLoc )
import Util		( zipEqual, zipWithEqual )
import Outputable
import FastString
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
  
dsLet (IPBinds binds) body
  = foldlDs dsIPBind body binds
  where
    dsIPBind body (n, e)
        = dsExpr e	`thenDs` \ e' ->
	  returnDs (Let (NonRec (ipNameName n) e') body)

-- Special case for bindings which bind unlifted variables
-- We need to do a case right away, rather than building
-- a tuple and doing selections.
-- Silently ignore INLINE pragmas...
dsLet bind@(MonoBind (AbsBinds [] [] exports inlines binds) sigs is_rec) body
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
	-> putSrcLocDs loc				$
	   matchWrapper (FunRhs (idName fun)) matches 	`thenDs` \ (args, rhs) ->
	   ASSERT( null args )	-- Functions aren't lifted
	   returnDs (bindNonRec fun rhs body_w_exports)

      PatMonoBind pat grhss loc
	-> putSrcLocDs loc			$
	   dsGuarded grhss 			`thenDs` \ rhs ->
	   mk_error_app pat			`thenDs` \ error_expr ->
	   matchSimply rhs PatBindRhs pat body_w_exports error_expr

      other -> pprPanic "dsLet: unlifted" (ppr bind $$ ppr body)
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

dsExpr (HsPar x) = dsExpr x
dsExpr (HsVar var)  = returnDs (Var var)
dsExpr (HsIPVar ip) = returnDs (Var (ipNameName ip))
dsExpr (HsLit lit)  = dsLit lit
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

dsExpr (HsSCC cc expr)
  = dsExpr expr			`thenDs` \ core_expr ->
    getModuleDs			`thenDs` \ mod_name ->
    returnDs (Note (SCC (mkUserCC cc mod_name)) core_expr)


-- hdaume: core annotation

dsExpr (HsCoreAnn fs expr)
  = dsExpr expr        `thenDs` \ core_expr ->
    returnDs (Note (CoreNote $ unpackFS fs) core_expr)

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

-- We need the `ListComp' form to use `deListComp' (rather than the "do" form)
-- because the interpretation of `stmts' depends on what sort of thing it is.
--
dsExpr (HsDo ListComp stmts _ result_ty src_loc)
  =	-- Special case for list comprehensions
    putSrcLocDs src_loc $
    dsListComp stmts elt_ty
  where
    (_, [elt_ty]) = tcSplitTyConApp result_ty

dsExpr (HsDo do_or_lc stmts ids result_ty src_loc)
  | isDoExpr do_or_lc
  = putSrcLocDs src_loc $
    dsDo do_or_lc stmts ids result_ty

dsExpr (HsDo PArrComp stmts _ result_ty src_loc)
  =	-- Special case for array comprehensions
    putSrcLocDs src_loc $
    dsPArrComp stmts elt_ty
  where
    (_, [elt_ty]) = tcSplitTyConApp result_ty

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

-- we create a list from the array elements and convert them into a list using
-- `PrelPArr.toP'
--
-- * the main disadvantage to this scheme is that `toP' traverses the list
--   twice: once to determine the length and a second time to put to elements
--   into the array; this inefficiency could be avoided by exposing some of
--   the innards of `PrelPArr' to the compiler (ie, have a `PrelPArrBase') so
--   that we can exploit the fact that we already know the length of the array
--   here at compile time
--
dsExpr (ExplicitPArr ty xs)
  = dsLookupGlobalId toPName				`thenDs` \toP      ->
    dsExpr (ExplicitList ty xs)				`thenDs` \coreList ->
    returnDs (mkApps (Var toP) [Type ty, coreList])

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

dsExpr (PArrSeqOut expr (FromTo from two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr two		  `thenDs` \ two2 ->
    returnDs (mkApps expr2 [from2, two2])

dsExpr (PArrSeqOut expr (FromThenTo from thn two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr thn		  `thenDs` \ thn2 ->
    dsExpr two		  `thenDs` \ two2 ->
    returnDs (mkApps expr2 [from2, thn2, two2])

dsExpr (PArrSeqOut expr _)
  = panic "DsExpr.dsExpr: Infinite parallel array!"
    -- the parser shouldn't have generated it and the renamer and typechecker
    -- shouldn't have let it through
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
	  = case [rhs | (sel_id,rhs) <- rbinds,
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
dsExpr (RecordUpdOut record_expr record_in_ty record_out_ty [])
  = dsExpr record_expr

dsExpr expr@(RecordUpdOut record_expr record_in_ty record_out_ty rbinds)
  = getSrcLocDs			`thenDs` \ src_loc ->
    dsExpr record_expr	 	`thenDs` \ record_expr' ->

	-- Desugar the rbinds, and generate let-bindings if
	-- necessary so that we don't lose sharing

    let
	in_inst_tys  = tcTyConAppArgs record_in_ty	-- Newtype opaque
	out_inst_tys = tcTyConAppArgs record_out_ty	-- Newtype opaque

	mk_val_arg field old_arg_id 
	  = case [rhs | (sel_id, rhs) <- rbinds, 
			field == recordSelectorFieldLabel sel_id] of
	      (rhs:rest) -> ASSERT(null rest) rhs
	      []	 -> HsVar old_arg_id

	mk_alt con
	  = newSysLocalsDs (dataConInstOrigArgTys con in_inst_tys) `thenDs` \ arg_ids ->
		-- This call to dataConArgTys won't work for existentials
	    let 
		val_args = zipWithEqual "dsExpr:RecordUpd" mk_val_arg
					(dataConFieldLabels con) arg_ids
		rhs = foldl HsApp (TyApp (HsVar (dataConWrapId con)) out_inst_tys)
				  val_args
	    in
	    returnDs (mkSimpleMatch [ConPatOut con (PrefixCon (map VarPat arg_ids)) record_in_ty [] []]
				    rhs
				    record_out_ty
				    src_loc)
    in
	-- Record stuff doesn't work for existentials
	-- The type checker checks for this, but we need 
	-- worry only about the constructors that are to be updated
    ASSERT2( all (not . isExistentialDataCon) cons_to_upd, ppr expr )

	-- It's important to generate the match with matchWrapper,
	-- and the right hand sides with applications of the wrapper Id
	-- so that everything works when we are doing fancy unboxing on the
	-- constructor aguments.
    mapDs mk_alt cons_to_upd		`thenDs` \ alts ->
    matchWrapper RecUpd alts		`thenDs` \ ([discrim_var], matching_code) ->

    returnDs (bindNonRec discrim_var record_expr' matching_code)

  where
    updated_fields :: [FieldLabel]
    updated_fields = [recordSelectorFieldLabel sel_id | (sel_id,_) <- rbinds]

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

Here is where we desugar the Template Haskell brackets and escapes

\begin{code}
-- Template Haskell stuff

#ifdef GHCI	/* Only if bootstrapping */
dsExpr (HsBracketOut x ps) = dsBracket x ps
dsExpr (HsReify r)	   = dsReify r
dsExpr (HsSplice n e _)    = pprPanic "dsExpr:splice" (ppr e)
#endif

-- Arrow notation extension
dsExpr (HsProc pat cmd src_loc) = dsProcExpr pat cmd src_loc
\end{code}


\begin{code}

#ifdef DEBUG
-- HsSyn constructs that just shouldn't be here:
dsExpr (ExprWithTySig _ _)  = panic "dsExpr:ExprWithTySig"
dsExpr (ArithSeqIn _)	    = panic "dsExpr:ArithSeqIn"
dsExpr (PArrSeqIn _)	    = panic "dsExpr:PArrSeqIn"
#endif

\end{code}

%--------------------------------------------------------------------

Basically does the translation given in the Haskell~1.3 report:

\begin{code}
dsDo	:: HsStmtContext Name
	-> [TypecheckedStmt]
	-> ReboundNames Id	-- id for: [return,fail,>>=,>>] and possibly mfixName
	-> Type			-- Element type; the whole expression has type (m t)
	-> DsM CoreExpr

dsDo do_or_lc stmts ids result_ty
  = dsReboundNames ids		`thenDs` \ (meth_binds, ds_meths) ->
    let
  	return_id = lookupReboundName ds_meths returnMName
	fail_id   = lookupReboundName ds_meths failMName
	bind_id   = lookupReboundName ds_meths bindMName
	then_id   = lookupReboundName ds_meths thenMName

	(m_ty, b_ty) = tcSplitAppTy result_ty	-- result_ty must be of the form (m b)
	is_do	     = isDoExpr do_or_lc 	-- True for both MDo and Do
	
	-- For ExprStmt, see the comments near HsExpr.Stmt about 
	-- exactly what ExprStmts mean!
	--
	-- In dsDo we can only see DoStmt and ListComp (no guards)

	go [ResultStmt expr locn]
	  | is_do     = do_expr expr locn
	  | otherwise = do_expr expr locn	`thenDs` \ expr2 ->
			returnDs (mkApps return_id [Type b_ty, expr2])

	go (ExprStmt expr a_ty locn : stmts)
	  | is_do	-- Do expression
	  = do_expr expr locn		`thenDs` \ expr2 ->
	    go stmts     		`thenDs` \ rest  ->
	    returnDs (mkApps then_id [Type a_ty, Type b_ty, expr2, rest])

	   | otherwise	-- List comprehension
	  = do_expr expr locn			`thenDs` \ expr2 ->
	    go stmts				`thenDs` \ rest ->
	    let
		msg = "Pattern match failure in do expression, " ++ showSDoc (ppr locn)
	    in
	    mkStringLit msg			`thenDs` \ core_msg ->
	    returnDs (mkIfThenElse expr2 rest 
				   (App (App fail_id (Type b_ty)) core_msg))
    
	go (LetStmt binds : stmts)
	  = go stmts 		`thenDs` \ rest   ->
	    dsLet binds	rest
	    
	go (BindStmt pat expr locn : stmts)
	  = go stmts			`thenDs` \ body -> 
	    putSrcLocDs locn 		$	-- Rest is associated with this location
	    dsExpr expr 	 	`thenDs` \ rhs ->
	    mkStringLit (mk_msg locn)	`thenDs` \ core_msg ->
	    let
		-- In a do expression, pattern-match failure just calls
		-- the monadic 'fail' rather than throwing an exception
		fail_expr  = mkApps fail_id [Type b_ty, core_msg]
		a_ty       = hsPatType pat
	    in
	    selectMatchVar pat		 	 		`thenDs` \ var ->
    	    matchSimply (Var var) (StmtCtxt do_or_lc) pat
			body fail_expr				`thenDs` \ match_code ->
	    returnDs (mkApps bind_id [Type a_ty, Type b_ty, rhs, Lam var match_code])

	go (RecStmt rec_stmts later_vars rec_vars rec_rets : stmts)
	  = go (bind_stmt : stmts)
	  where
	    bind_stmt = dsRecStmt m_ty ds_meths rec_stmts later_vars rec_vars rec_rets
	    
    in
    go stmts				`thenDs` \ stmts_code ->
    returnDs (foldr Let stmts_code meth_binds)

  where
    do_expr expr locn = putSrcLocDs locn (dsExpr expr)
    mk_msg locn = "Pattern match failure in do expression at " ++ showSDoc (ppr locn)
\end{code}

Translation for RecStmt's: 
-----------------------------
We turn (RecStmt [v1,..vn] stmts) into:
  
  (v1,..,vn) <- mfix (\~(v1,..vn). do stmts
				      return (v1,..vn))

\begin{code}
dsRecStmt :: Type		-- Monad type constructor :: * -> *
	  -> [(Name,Id)]	-- Rebound Ids
	  -> [TypecheckedStmt]
  	  -> [Id] -> [Id] -> [TypecheckedHsExpr]
	  -> TypecheckedStmt
dsRecStmt m_ty ds_meths stmts later_vars rec_vars rec_rets
  = ASSERT( length vars == length rets )
    BindStmt tup_pat mfix_app noSrcLoc
  where 
	vars@(var1:rest) = later_vars           ++ rec_vars		-- Always at least one
	rets@(ret1:_)    = map HsVar later_vars ++ rec_rets
	one_var          = null rest

	mfix_app = HsApp (TyApp (HsVar mfix_id) [tup_ty]) mfix_arg
	mfix_arg = HsLam (mkSimpleMatch [tup_pat] body tup_ty noSrcLoc)

	tup_expr | one_var   = ret1
		 | otherwise = ExplicitTuple rets Boxed
	tup_ty   	     = mkCoreTupTy (map idType vars)
					-- Deals with singleton case
	tup_pat  | one_var   = VarPat var1
		 | otherwise = LazyPat (TuplePat (map VarPat vars) Boxed)

	body = HsDo DoExpr (stmts ++ [return_stmt]) 
			   [(n, HsVar id) | (n,id) <- ds_meths]	-- A bit of a hack
			   (mkAppTy m_ty tup_ty)
			   noSrcLoc

  	Var return_id = lookupReboundName ds_meths returnMName
	Var mfix_id   = lookupReboundName ds_meths mfixName

	return_stmt = ResultStmt return_app noSrcLoc
	return_app  = HsApp (TyApp (HsVar return_id) [tup_ty]) tup_expr
\end{code}
