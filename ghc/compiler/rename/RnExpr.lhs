%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnExpr]{Renaming of expressions}

Basically dependency analysis.

Handles @Match@, @GRHSs@, @HsExpr@, and @Qualifier@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.

\begin{code}
module RnExpr (
	rnMatch, rnGRHSs, rnPat, rnExpr, rnExprs,
	checkPrecMatch
   ) where

#include "HsVersions.h"

import {-# SOURCE #-} RnBinds  ( rnBinds ) 
import {-# SOURCE #-} RnSource ( rnHsSigType, rnHsType )

import HsSyn
import RdrHsSyn
import RnHsSyn
import RnMonad
import RnEnv
import RnIfaces		( lookupFixity )
import CmdLineOpts	( opt_GlasgowExts, opt_IgnoreAsserts )
import BasicTypes	( Fixity(..), FixityDirection(..), defaultFixity )
import PrelInfo		( numClass_RDR, fractionalClass_RDR, eqClass_RDR, 
			  ccallableClass_RDR, creturnableClass_RDR, 
			  monadClass_RDR, enumClass_RDR, ordClass_RDR,
			  ratioDataCon_RDR, negate_RDR, assertErr_RDR,
			  ioDataCon_RDR
			)
import TysPrim		( charPrimTyCon, addrPrimTyCon, intPrimTyCon, 
			  floatPrimTyCon, doublePrimTyCon
			)
import Name		( nameUnique, isLocallyDefined, NamedThing(..)
                        , mkSysLocalName, nameSrcLoc
			)
import NameSet
import UniqFM		( isNullUFM )
import FiniteMap	( elemFM )
import UniqSet		( emptyUniqSet, UniqSet )
import Unique		( assertIdKey )
import Util		( removeDups )
import ListSetOps	( unionLists )
import Maybes		( maybeToBool )
import Outputable
\end{code}


*********************************************************
*							*
\subsection{Patterns}
*							*
*********************************************************

\begin{code}
rnPat :: RdrNamePat -> RnMS (RenamedPat, FreeVars)

rnPat WildPatIn = returnRn (WildPatIn, emptyFVs)

rnPat (VarPatIn name)
  = lookupBndrRn  name			`thenRn` \ vname ->
    returnRn (VarPatIn vname, emptyFVs)

rnPat (SigPatIn pat ty)
  | opt_GlasgowExts
  = rnPat pat		`thenRn` \ (pat', fvs1) ->
    rnHsType doc ty	`thenRn` \ (ty',  fvs2) ->
    returnRn (SigPatIn pat' ty', fvs1 `plusFV` fvs2)

  | otherwise
  = addErrRn (patSigErr ty)	`thenRn_`
    rnPat pat
  where
    doc = text "a pattern type-signature"
    
rnPat (LitPatIn lit) 
  = litOccurrence lit			`thenRn` \ fvs1 ->
    lookupImplicitOccRn eqClass_RDR	`thenRn` \ eq   ->	-- Needed to find equality on pattern
    returnRn (LitPatIn lit, fvs1 `addOneFV` eq)

rnPat (LazyPatIn pat)
  = rnPat pat		`thenRn` \ (pat', fvs) ->
    returnRn (LazyPatIn pat', fvs)

rnPat (AsPatIn name pat)
  = rnPat pat		`thenRn` \ (pat', fvs) ->
    lookupBndrRn name	`thenRn` \ vname ->
    returnRn (AsPatIn vname pat', fvs)

rnPat (ConPatIn con pats)
  = lookupOccRn con		`thenRn` \ con' ->
    mapFvRn rnPat pats  	`thenRn` \ (patslist, fvs) ->
    returnRn (ConPatIn con' patslist, fvs `addOneFV` con')

rnPat (ConOpPatIn pat1 con _ pat2)
  = rnPat pat1		`thenRn` \ (pat1', fvs1) ->
    lookupOccRn con	`thenRn` \ con' ->
    rnPat pat2		`thenRn` \ (pat2', fvs2) ->

    getModeRn		`thenRn` \ mode ->
	-- See comments with rnExpr (OpApp ...)
    (case mode of
	InterfaceMode -> returnRn (ConOpPatIn pat1' con' defaultFixity pat2')
	SourceMode    -> lookupFixity con'	`thenRn` \ fixity ->
			 mkConOpPatRn pat1' con' fixity pat2'
    )								`thenRn` \ pat' ->
    returnRn (pat', fvs1 `plusFV` fvs2 `addOneFV` con')

-- Negated patters can only be literals, and they are dealt with
-- by negating the literal at compile time, not by using the negation
-- operation in Num.  So we don't need to make an implicit reference
-- to negate_RDR.
rnPat neg@(NegPatIn pat)
  = checkRn (valid_neg_pat pat) (negPatErr neg)
			`thenRn_`
    rnPat pat		`thenRn` \ (pat', fvs) ->
    returnRn (NegPatIn pat', fvs)
  where
    valid_neg_pat (LitPatIn (HsInt  _)) = True
    valid_neg_pat (LitPatIn (HsFrac _)) = True
    valid_neg_pat _                     = False

rnPat (ParPatIn pat)
  = rnPat pat		`thenRn` \ (pat', fvs) ->
    returnRn (ParPatIn pat', fvs)

rnPat (NPlusKPatIn name lit)
  = litOccurrence lit			`thenRn` \ fvs ->
    lookupImplicitOccRn ordClass_RDR	`thenRn` \ ord ->
    lookupBndrRn name			`thenRn` \ name' ->
    returnRn (NPlusKPatIn name' lit, fvs `addOneFV` ord)

rnPat (ListPatIn pats)
  = mapFvRn rnPat pats			`thenRn` \ (patslist, fvs) ->
    returnRn (ListPatIn patslist, fvs `addOneFV` listTyCon_name)

rnPat (TuplePatIn pats boxed)
  = mapFvRn rnPat pats					   `thenRn` \ (patslist, fvs) ->
    returnRn (TuplePatIn patslist boxed, fvs `addOneFV` tycon_name)
  where
    tycon_name = tupleTyCon_name boxed (length pats)

rnPat (RecPatIn con rpats)
  = lookupOccRn con 	`thenRn` \ con' ->
    rnRpats rpats	`thenRn` \ (rpats', fvs) ->
    returnRn (RecPatIn con' rpats', fvs `addOneFV` con')
\end{code}

************************************************************************
*									*
\subsection{Match}
*									*
************************************************************************

\begin{code}
rnMatch :: RdrNameMatch -> RnMS (RenamedMatch, FreeVars)

rnMatch match@(Match _ pats maybe_rhs_sig grhss)
  = pushSrcLocRn (getMatchLoc match)	$

	-- Find the universally quantified type variables
	-- in the pattern type signatures
    getLocalNameEnv			`thenRn` \ name_env ->
    let
	tyvars_in_sigs = rhs_sig_tyvars `unionLists` tyvars_in_pats
	rhs_sig_tyvars = case maybe_rhs_sig of
				Nothing -> []
				Just ty -> extractHsTyRdrNames ty
	tyvars_in_pats = extractPatsTyVars pats
	forall_tyvars  = filter (not . (`elemFM` name_env)) tyvars_in_sigs
	doc            = text "a pattern type-signature"
    in
    bindTyVarsFVRn doc (map UserTyVar forall_tyvars)	$ \ sig_tyvars ->

	-- Note that we do a single bindLocalsRn for all the
	-- matches together, so that we spot the repeated variable in
	--	f x x = 1
    bindLocalsFVRn doc (collectPatsBinders pats) $ \ new_binders ->

    mapFvRn rnPat pats			`thenRn` \ (pats', pat_fvs) ->
    rnGRHSs grhss			`thenRn` \ (grhss', grhss_fvs) ->
    (case maybe_rhs_sig of
	Nothing -> returnRn (Nothing, emptyFVs)
	Just ty | opt_GlasgowExts -> rnHsType doc ty	`thenRn` \ (ty', ty_fvs) ->
				     returnRn (Just ty', ty_fvs)
		| otherwise	  -> addErrRn (patSigErr ty)	`thenRn_`
				     returnRn (Nothing, emptyFVs)
    )					`thenRn` \ (maybe_rhs_sig', ty_fvs) ->

    let
	binder_set     = mkNameSet new_binders
	unused_binders = nameSetToList (binder_set `minusNameSet` grhss_fvs)
	all_fvs	       = grhss_fvs `plusFV` pat_fvs `plusFV` ty_fvs
    in
    warnUnusedMatches unused_binders		`thenRn_`
    
    returnRn (Match sig_tyvars pats' maybe_rhs_sig' grhss', all_fvs)
	-- The bindLocals and bindTyVars will remove the bound FVs
\end{code}

%************************************************************************
%*									*
\subsubsection{Guarded right-hand sides (GRHSs)}
%*									*
%************************************************************************

\begin{code}
rnGRHSs :: RdrNameGRHSs -> RnMS (RenamedGRHSs, FreeVars)

rnGRHSs (GRHSs grhss binds maybe_ty)
  = ASSERT( not (maybeToBool maybe_ty) )
    rnBinds binds		$ \ binds' ->
    mapFvRn rnGRHS grhss	`thenRn` \ (grhss', fvGRHSs) ->
    returnRn (GRHSs grhss' binds' Nothing, fvGRHSs)

rnGRHS (GRHS guarded locn)
  = pushSrcLocRn locn $		    
    (if not (opt_GlasgowExts || is_standard_guard guarded) then
		addWarnRn (nonStdGuardErr guarded)
     else
		returnRn ()
    )		`thenRn_`

    rnStmts rnExpr guarded	`thenRn` \ (guarded', fvs) ->
    returnRn (GRHS guarded' locn, fvs)
  where
	-- Standard Haskell 1.4 guards are just a single boolean
	-- expression, rather than a list of qualifiers as in the
	-- Glasgow extension
    is_standard_guard [ExprStmt _ _]                = True
    is_standard_guard [GuardStmt _ _, ExprStmt _ _] = True
    is_standard_guard other	      		    = False
\end{code}

%************************************************************************
%*									*
\subsubsection{Expressions}
%*									*
%************************************************************************

\begin{code}
rnExprs :: [RdrNameHsExpr] -> RnMS ([RenamedHsExpr], FreeVars)
rnExprs ls = rnExprs' ls emptyUniqSet
 where
  rnExprs' [] acc = returnRn ([], acc)
  rnExprs' (expr:exprs) acc
   = rnExpr expr 	        `thenRn` \ (expr', fvExpr) ->

	-- Now we do a "seq" on the free vars because typically it's small
	-- or empty, especially in very long lists of constants
    let
	acc' = acc `plusFV` fvExpr
    in
    (grubby_seqNameSet acc' rnExprs') exprs acc'	`thenRn` \ (exprs', fvExprs) ->
    returnRn (expr':exprs', fvExprs)

-- Grubby little function to do "seq" on namesets; replace by proper seq when GHC can do seq
grubby_seqNameSet ns result | isNullUFM ns = result
			    | otherwise    = result
\end{code}

Variables. We look up the variable and return the resulting name. 

\begin{code}
rnExpr :: RdrNameHsExpr -> RnMS (RenamedHsExpr, FreeVars)

rnExpr (HsVar v)
  = lookupOccRn v	`thenRn` \ name ->
    if nameUnique name == assertIdKey then
	-- We expand it to (GHCerr.assert__ location)
        mkAssertExpr
    else
        -- The normal case
       returnRn (HsVar name, unitFV name)

rnExpr (HsLit lit) 
  = litOccurrence lit		`thenRn` \ fvs ->
    returnRn (HsLit lit, fvs)

rnExpr (HsLam match)
  = rnMatch match	`thenRn` \ (match', fvMatch) ->
    returnRn (HsLam match', fvMatch)

rnExpr (HsApp fun arg)
  = rnExpr fun		`thenRn` \ (fun',fvFun) ->
    rnExpr arg		`thenRn` \ (arg',fvArg) ->
    returnRn (HsApp fun' arg', fvFun `plusFV` fvArg)

rnExpr (OpApp e1 op _ e2) 
  = rnExpr e1				`thenRn` \ (e1', fv_e1) ->
    rnExpr e2				`thenRn` \ (e2', fv_e2) ->
    rnExpr op				`thenRn` \ (op'@(HsVar op_name), fv_op) ->

	-- Deal with fixity
	-- When renaming code synthesised from "deriving" declarations
	-- we're in Interface mode, and we should ignore fixity; assume
	-- that the deriving code generator got the association correct
	-- Don't even look up the fixity when in interface mode
    getModeRn				`thenRn` \ mode -> 
    (case mode of
	SourceMode    -> lookupFixity op_name		`thenRn` \ fixity ->
			 mkOpAppRn e1' op' fixity e2'
	InterfaceMode -> returnRn (OpApp e1' op' defaultFixity e2')
    )					`thenRn` \ final_e -> 

    returnRn (final_e,
	      fv_e1 `plusFV` fv_op `plusFV` fv_e2)

rnExpr (NegApp e n)
  = rnExpr e				`thenRn` \ (e', fv_e) ->
    lookupImplicitOccRn negate_RDR	`thenRn` \ neg ->
    mkNegAppRn e' (HsVar neg)		`thenRn` \ final_e ->
    returnRn (final_e, fv_e `addOneFV` neg)

rnExpr (HsPar e)
  = rnExpr e 		`thenRn` \ (e', fvs_e) ->
    returnRn (HsPar e', fvs_e)

rnExpr (SectionL expr op)
  = rnExpr expr	 	`thenRn` \ (expr', fvs_expr) ->
    rnExpr op	 	`thenRn` \ (op', fvs_op) ->
    returnRn (SectionL expr' op', fvs_op `plusFV` fvs_expr)

rnExpr (SectionR op expr)
  = rnExpr op	 	`thenRn` \ (op',   fvs_op) ->
    rnExpr expr	 	`thenRn` \ (expr', fvs_expr) ->
    returnRn (SectionR op' expr', fvs_op `plusFV` fvs_expr)

rnExpr (CCall fun args may_gc is_casm fake_result_ty)
	-- Check out the comment on RnIfaces.getNonWiredDataDecl about ccalls
  = lookupImplicitOccRn ccallableClass_RDR	`thenRn` \ cc ->
    lookupImplicitOccRn creturnableClass_RDR	`thenRn` \ cr ->
    lookupImplicitOccRn ioDataCon_RDR		`thenRn` \ io ->
    rnExprs args				`thenRn` \ (args', fvs_args) ->
    returnRn (CCall fun args' may_gc is_casm fake_result_ty, 
	      fvs_args `addOneFV` cc `addOneFV` cr `addOneFV` io)

rnExpr (HsSCC label expr)
  = rnExpr expr	 	`thenRn` \ (expr', fvs_expr) ->
    returnRn (HsSCC label expr', fvs_expr)

rnExpr (HsCase expr ms src_loc)
  = pushSrcLocRn src_loc $
    rnExpr expr		 	`thenRn` \ (new_expr, e_fvs) ->
    mapFvRn rnMatch ms		`thenRn` \ (new_ms, ms_fvs) ->
    returnRn (HsCase new_expr new_ms src_loc, e_fvs `plusFV` ms_fvs)

rnExpr (HsLet binds expr)
  = rnBinds binds		$ \ binds' ->
    rnExpr expr			 `thenRn` \ (expr',fvExpr) ->
    returnRn (HsLet binds' expr', fvExpr)

rnExpr (HsDo do_or_lc stmts src_loc)
  = pushSrcLocRn src_loc $
    lookupImplicitOccRn monadClass_RDR		`thenRn` \ monad ->
    rnStmts rnExpr stmts			`thenRn` \ (stmts', fvs) ->
    returnRn (HsDo do_or_lc stmts' src_loc, fvs `addOneFV` monad)

rnExpr (ExplicitList exps)
  = rnExprs exps		 	`thenRn` \ (exps', fvs) ->
    returnRn  (ExplicitList exps', fvs `addOneFV` listTyCon_name)

rnExpr (ExplicitTuple exps boxed)
  = rnExprs exps	 			`thenRn` \ (exps', fvs) ->
    returnRn (ExplicitTuple exps' boxed, fvs `addOneFV` tycon_name)
  where
    tycon_name = tupleTyCon_name boxed (length exps)

rnExpr (RecordCon con_id rbinds)
  = lookupOccRn con_id 			`thenRn` \ conname ->
    rnRbinds "construction" rbinds	`thenRn` \ (rbinds', fvRbinds) ->
    returnRn (RecordCon conname rbinds', fvRbinds `addOneFV` conname)

rnExpr (RecordUpd expr rbinds)
  = rnExpr expr			`thenRn` \ (expr', fvExpr) ->
    rnRbinds "update" rbinds	`thenRn` \ (rbinds', fvRbinds) ->
    returnRn (RecordUpd expr' rbinds', fvExpr `plusFV` fvRbinds)

rnExpr (ExprWithTySig expr pty)
  = rnExpr expr			 		`thenRn` \ (expr', fvExpr) ->
    rnHsSigType (text "an expression") pty	`thenRn` \ (pty', fvTy) ->
    returnRn (ExprWithTySig expr' pty', fvExpr `plusFV` fvTy)

rnExpr (HsIf p b1 b2 src_loc)
  = pushSrcLocRn src_loc $
    rnExpr p		`thenRn` \ (p', fvP) ->
    rnExpr b1		`thenRn` \ (b1', fvB1) ->
    rnExpr b2		`thenRn` \ (b2', fvB2) ->
    returnRn (HsIf p' b1' b2' src_loc, plusFVs [fvP, fvB1, fvB2])

rnExpr (ArithSeqIn seq)
  = lookupImplicitOccRn enumClass_RDR	`thenRn` \ enum ->
    rn_seq seq	 			`thenRn` \ (new_seq, fvs) ->
    returnRn (ArithSeqIn new_seq, fvs `addOneFV` enum)
  where
    rn_seq (From expr)
     = rnExpr expr 	`thenRn` \ (expr', fvExpr) ->
       returnRn (From expr', fvExpr)

    rn_seq (FromThen expr1 expr2)
     = rnExpr expr1 	`thenRn` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenRn` \ (expr2', fvExpr2) ->
       returnRn (FromThen expr1' expr2', fvExpr1 `plusFV` fvExpr2)

    rn_seq (FromTo expr1 expr2)
     = rnExpr expr1	`thenRn` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenRn` \ (expr2', fvExpr2) ->
       returnRn (FromTo expr1' expr2', fvExpr1 `plusFV` fvExpr2)

    rn_seq (FromThenTo expr1 expr2 expr3)
     = rnExpr expr1	`thenRn` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenRn` \ (expr2', fvExpr2) ->
       rnExpr expr3	`thenRn` \ (expr3', fvExpr3) ->
       returnRn (FromThenTo expr1' expr2' expr3',
		  plusFVs [fvExpr1, fvExpr2, fvExpr3])
\end{code}

%************************************************************************
%*									*
\subsubsection{@Rbinds@s and @Rpats@s: in record expressions}
%*									*
%************************************************************************

\begin{code}
rnRbinds str rbinds 
  = mapRn_ field_dup_err dup_fields	`thenRn_`
    mapFvRn rn_rbind rbinds		`thenRn` \ (rbinds', fvRbind) ->
    returnRn (rbinds', fvRbind)
  where
    (_, dup_fields) = removeDups compare [ f | (f,_,_) <- rbinds ]

    field_dup_err dups = addErrRn (dupFieldErr str dups)

    rn_rbind (field, expr, pun)
      = lookupGlobalOccRn field	`thenRn` \ fieldname ->
	rnExpr expr		`thenRn` \ (expr', fvExpr) ->
	returnRn ((fieldname, expr', pun), fvExpr `addOneFV` fieldname)

rnRpats rpats
  = mapRn_ field_dup_err dup_fields 	`thenRn_`
    mapFvRn rn_rpat rpats		`thenRn` \ (rpats', fvs) ->
    returnRn (rpats', fvs)
  where
    (_, dup_fields) = removeDups compare [ f | (f,_,_) <- rpats ]

    field_dup_err dups = addErrRn (dupFieldErr "pattern" dups)

    rn_rpat (field, pat, pun)
      = lookupGlobalOccRn field	`thenRn` \ fieldname ->
	rnPat pat		`thenRn` \ (pat', fvs) ->
	returnRn ((fieldname, pat', pun), fvs `addOneFV` fieldname)
\end{code}

%************************************************************************
%*									*
\subsubsection{@Stmt@s: in @do@ expressions}
%*									*
%************************************************************************

Note that although some bound vars may appear in the free var set for
the first qual, these will eventually be removed by the caller. For
example, if we have @[p | r <- s, q <- r, p <- q]@, when doing
@[q <- r, p <- q]@, the free var set for @q <- r@ will
be @{r}@, and the free var set for the entire Quals will be @{r}@. This
@r@ will be removed only when we finally return from examining all the
Quals.

\begin{code}
type RnExprTy = RdrNameHsExpr -> RnMS (RenamedHsExpr, FreeVars)

rnStmts :: RnExprTy
	-> [RdrNameStmt] 
	-> RnMS ([RenamedStmt], FreeVars)

rnStmts rn_expr []
  = returnRn ([], emptyFVs)

rnStmts rn_expr (stmt:stmts)
  = rnStmt rn_expr stmt				$ \ stmt' ->
    rnStmts rn_expr stmts			`thenRn` \ (stmts', fvs) ->
    returnRn (stmt' : stmts', fvs)

rnStmt :: RnExprTy -> RdrNameStmt
       -> (RenamedStmt -> RnMS (a, FreeVars))
       -> RnMS (a, FreeVars)
-- Because of mutual recursion we have to pass in rnExpr.

rnStmt rn_expr (BindStmt pat expr src_loc) thing_inside
  = pushSrcLocRn src_loc $
    rn_expr expr		 			`thenRn` \ (expr', fv_expr) ->
    bindLocalsFVRn doc binders				$ \ new_binders ->
    rnPat pat					 	`thenRn` \ (pat', fv_pat) ->
    thing_inside (BindStmt pat' expr' src_loc)		`thenRn` \ (result, fvs) -> 
    returnRn (result, fv_expr `plusFV` fvs `plusFV` fv_pat)
  where
    binders = collectPatBinders pat
    doc = text "a pattern in do binding" 

rnStmt rn_expr (ExprStmt expr src_loc) thing_inside
  = pushSrcLocRn src_loc $
    rn_expr expr 				`thenRn` \ (expr', fv_expr) ->
    thing_inside (ExprStmt expr' src_loc)	`thenRn` \ (result, fvs) ->
    returnRn (result, fv_expr `plusFV` fvs)

rnStmt rn_expr (GuardStmt expr src_loc) thing_inside
  = pushSrcLocRn src_loc $
    rn_expr expr 				`thenRn` \ (expr', fv_expr) ->
    thing_inside (GuardStmt expr' src_loc)	`thenRn` \ (result, fvs) ->
    returnRn (result, fv_expr `plusFV` fvs)

rnStmt rn_expr (ReturnStmt expr) thing_inside
  = rn_expr expr				`thenRn` \ (expr', fv_expr) ->
    thing_inside (ReturnStmt expr')		`thenRn` \ (result, fvs) ->
    returnRn (result, fv_expr `plusFV` fvs)

rnStmt rn_expr (LetStmt binds) thing_inside
  = rnBinds binds		$ \ binds' ->
    thing_inside (LetStmt binds')
\end{code}

%************************************************************************
%*									*
\subsubsection{Precedence Parsing}
%*									*
%************************************************************************

@mkOpAppRn@ deals with operator fixities.  The argument expressions
are assumed to be already correctly arranged.  It needs the fixities
recorded in the OpApp nodes, because fixity info applies to the things
the programmer actually wrote, so you can't find it out from the Name.

Furthermore, the second argument is guaranteed not to be another
operator application.  Why? Because the parser parses all
operator appications left-associatively.

\begin{code}
mkOpAppRn :: RenamedHsExpr -> RenamedHsExpr -> Fixity -> RenamedHsExpr
	  -> RnMS RenamedHsExpr

mkOpAppRn e1@(OpApp e11 op1 fix1 e12) 
	  op2 fix2 e2
  | nofix_error
  = addErrRn (precParseErr (get op1,fix1) (get op2,fix2))	`thenRn_`
    returnRn (OpApp e1 op2 fix2 e2)

  | rearrange_me
  = mkOpAppRn e12 op2 fix2 e2		`thenRn` \ new_e ->
    returnRn (OpApp e11 op1 fix1 new_e)
  where
    (nofix_error, rearrange_me) = compareFixity fix1 fix2

mkOpAppRn e1@(NegApp neg_arg neg_op) 
	  op2 
	  fix2@(Fixity prec2 dir2)
	  e2
  | nofix_error
  = addErrRn (precParseErr (get neg_op,fix_neg) (get op2,fix2))	`thenRn_`
    returnRn (OpApp e1 op2 fix2 e2)

  | rearrange_me
  = mkOpAppRn neg_arg op2 fix2 e2	`thenRn` \ new_e ->
    returnRn (NegApp new_e neg_op)
  where
    fix_neg = Fixity 6 InfixL  	-- Precedence of unary negate is wired in as infixl 6!
    (nofix_error, rearrange_me) = compareFixity fix_neg fix2

mkOpAppRn e1 op fix e2 			-- Default case, no rearrangment
  = ASSERT( if right_op_ok fix e2 then True
	    else pprPanic "mkOpAppRn" (vcat [ppr e1, text "---", ppr op, 
					     text "---", ppr fix, text "---", ppr e2])
    )
    returnRn (OpApp e1 op fix e2)

get (HsVar n) = n

-- Parser left-associates everything, but 
-- derived instances may have correctly-associated things to
-- in the right operarand.  So we just check that the right operand is OK
right_op_ok fix1 (OpApp _ _ fix2 _)
  = not error_please && associate_right
  where
    (error_please, associate_right) = compareFixity fix1 fix2
right_op_ok fix1 other
  = True

-- Parser initially makes negation bind more tightly than any other operator
mkNegAppRn neg_arg neg_op
  = 
#ifdef DEBUG
    getModeRn			`thenRn` \ mode ->
    ASSERT( not_op_app mode neg_arg )
#endif
    returnRn (NegApp neg_arg neg_op)

not_op_app SourceMode (OpApp _ _ _ _) = False
not_op_app mode other	 	      = True
\end{code}

\begin{code}
mkConOpPatRn :: RenamedPat -> Name -> Fixity -> RenamedPat
	     -> RnMS RenamedPat

mkConOpPatRn p1@(ConOpPatIn p11 op1 fix1 p12) 
	     op2 fix2 p2
  | nofix_error
  = addErrRn (precParseErr (op1,fix1) (op2,fix2))	`thenRn_`
    returnRn (ConOpPatIn p1 op2 fix2 p2)

  | rearrange_me
  = mkConOpPatRn p12 op2 fix2 p2		`thenRn` \ new_p ->
    returnRn (ConOpPatIn p11 op1 fix1 new_p)

  where
    (nofix_error, rearrange_me) = compareFixity fix1 fix2

mkConOpPatRn p1@(NegPatIn neg_arg) 
	  op2 
	  fix2@(Fixity prec2 dir2)
	  p2
  | prec2 > 6 	-- Precedence of unary - is wired in as 6!
  = addErrRn (precParseNegPatErr (op2,fix2))	`thenRn_`
    returnRn (ConOpPatIn p1 op2 fix2 p2)

mkConOpPatRn p1 op fix p2 			-- Default case, no rearrangment
  = ASSERT( not_op_pat p2 )
    returnRn (ConOpPatIn p1 op fix p2)

not_op_pat (ConOpPatIn _ _ _ _) = False
not_op_pat other   	        = True
\end{code}

\begin{code}
checkPrecMatch :: Bool -> Name -> RenamedMatch -> RnMS ()

checkPrecMatch False fn match
  = returnRn ()

checkPrecMatch True op (Match _ [p1,p2] _ _)
  = getModeRn 		`thenRn` \ mode ->
	-- See comments with rnExpr (OpApp ...)
    case mode of
	InterfaceMode -> returnRn ()
	SourceMode    -> checkPrec op p1 False	`thenRn_`
			 checkPrec op p2 True

checkPrecMatch True op _ = panic "checkPrecMatch"

checkPrec op (ConOpPatIn _ op1 _ _) right
  = lookupFixity op	`thenRn` \  op_fix@(Fixity op_prec  op_dir) ->
    lookupFixity op1	`thenRn` \ op1_fix@(Fixity op1_prec op1_dir) ->
    let
	inf_ok = op1_prec > op_prec || 
	         (op1_prec == op_prec &&
		  (op1_dir == InfixR && op_dir == InfixR && right ||
		   op1_dir == InfixL && op_dir == InfixL && not right))

	info  = (op,op_fix)
	info1 = (op1,op1_fix)
	(infol, infor) = if right then (info, info1) else (info1, info)
    in
    checkRn inf_ok (precParseErr infol infor)

checkPrec op (NegPatIn _) right
  = lookupFixity op	`thenRn` \ op_fix@(Fixity op_prec op_dir) ->
    checkRn (op_prec <= 6) (precParseNegPatErr (op,op_fix))

checkPrec op pat right
  = returnRn ()
\end{code}

Consider
	a `op1` b `op2` c

(compareFixity op1 op2) tells which way to arrange appication, or
whether there's an error.

\begin{code}
compareFixity :: Fixity -> Fixity
	      -> (Bool,		-- Error please
		  Bool)		-- Associate to the right: a op1 (b op2 c)
compareFixity (Fixity prec1 dir1) (Fixity prec2 dir2)
  = case prec1 `compare` prec2 of
	GT -> left
	LT -> right
	EQ -> case (dir1, dir2) of
			(InfixR, InfixR) -> right
			(InfixL, InfixL) -> left
			_		 -> error_please
  where
    right	 = (False, True)
    left         = (False, False)
    error_please = (True,  False)
\end{code}

%************************************************************************
%*									*
\subsubsection{Literals}
%*									*
%************************************************************************

When literals occur we have to make sure that the types and classes they involve
are made available.

\begin{code}
litOccurrence (HsChar _)
  = returnRn (unitFV charTyCon_name)

litOccurrence (HsCharPrim _)
  = returnRn (unitFV (getName charPrimTyCon))

litOccurrence (HsString _)
  = returnRn (unitFV listTyCon_name `plusFV` unitFV charTyCon_name)

litOccurrence (HsStringPrim _)
  = returnRn (unitFV (getName addrPrimTyCon))

litOccurrence (HsInt _)
  = lookupImplicitOccRn numClass_RDR `thenRn` \ num ->
    returnRn (unitFV num)			-- Int and Integer are forced in by Num

litOccurrence (HsFrac _)
  = lookupImplicitOccRn fractionalClass_RDR	`thenRn` \ frac ->
    lookupImplicitOccRn ratioDataCon_RDR	`thenRn` \ ratio ->
    returnRn (unitFV frac `plusFV` unitFV ratio)
	-- We have to make sure that the Ratio type is imported with
	-- its constructor, because literals of type Ratio t are
	-- built with that constructor.
	-- The Rational type is needed too, but that will come in
	-- when fractionalClass does.
    
litOccurrence (HsIntPrim _)
  = returnRn (unitFV (getName intPrimTyCon))

litOccurrence (HsFloatPrim _)
  = returnRn (unitFV (getName floatPrimTyCon))

litOccurrence (HsDoublePrim _)
  = returnRn (unitFV (getName doublePrimTyCon))

litOccurrence (HsLitLit _)
  = lookupImplicitOccRn ccallableClass_RDR	`thenRn` \ cc ->
    returnRn (unitFV cc)
\end{code}

%************************************************************************
%*									*
\subsubsection{Assertion utils}
%*									*
%************************************************************************

\begin{code}
mkAssertExpr :: RnMS (RenamedHsExpr, FreeVars)
mkAssertExpr =
  mkImportedGlobalFromRdrName assertErr_RDR		`thenRn` \ name ->
  getSrcLocRn           				`thenRn` \ sloc ->

    -- if we're ignoring asserts, return (\ _ e -> e)
    -- if not, return (assertError "src-loc")

  if opt_IgnoreAsserts then
    getUniqRn				`thenRn` \ uniq ->
    let
     vname = mkSysLocalName uniq SLIT("v")
     expr  = HsLam ignorePredMatch
     loc   = nameSrcLoc vname
     ignorePredMatch = Match [] [WildPatIn, VarPatIn vname] Nothing 
                             (GRHSs [GRHS [ExprStmt (HsVar vname) loc] loc]
			            EmptyBinds Nothing)
    in
    returnRn (expr, unitFV name)
  else
    let
     expr = 
          HsApp (HsVar name)
	        (HsLit (HsString (_PK_ (showSDoc (ppr sloc)))))

    in
    returnRn (expr, unitFV name)

\end{code}

%************************************************************************
%*									*
\subsubsection{Errors}
%*									*
%************************************************************************

\begin{code}
dupFieldErr str (dup:rest)
  = hsep [ptext SLIT("duplicate field name"), 
          quotes (ppr dup),
	  ptext SLIT("in record"), text str]

negPatErr pat 
  = sep [ptext SLIT("prefix `-' not applied to literal in pattern"), quotes (ppr pat)]

precParseNegPatErr op 
  = hang (ptext SLIT("precedence parsing error"))
      4 (hsep [ptext SLIT("prefix `-' has lower precedence than"), 
	       quotes (pp_op op), 
	       ptext SLIT("in pattern")])

precParseErr op1 op2 
  = hang (ptext SLIT("precedence parsing error"))
      4 (hsep [ptext SLIT("cannot mix"), quotes (pp_op op1), ptext SLIT("and"), 
	       quotes (pp_op op2),
	       ptext SLIT("in the same infix expression")])

nonStdGuardErr guard
  = hang (ptext SLIT("accepting non-standard pattern guards (-fglasgow-exts to suppress this message)"))
      4 (ppr guard)

patSigErr ty
  = hang (ptext SLIT("Illegal signature in pattern:") <+> ppr ty)
	 4 (ptext SLIT("Use -fglasgow-exts to permit it"))

pp_op (op, fix) = hcat [ppr op, space, parens (ppr fix)]
\end{code}
