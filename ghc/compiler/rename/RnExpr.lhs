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
	rnStmt, rnStmts, checkPrecMatch
   ) where

#include "HsVersions.h"

import {-# SOURCE #-} RnSource  ( rnSrcDecls, rnBinds ) 

import HsSyn
import RdrHsSyn
import RnHsSyn
import TcRnMonad
import RnEnv
import RnTypes		( rnHsTypeFVs, precParseErr, sectionPrecErr )
import CmdLineOpts	( DynFlag(..), opt_IgnoreAsserts )
import Literal		( inIntRange, inCharRange )
import BasicTypes	( Fixity(..), FixityDirection(..), IPName(..),
			  defaultFixity, negateFixity, compareFixity )
import PrelNames	( hasKey, assertIdKey, 
			  eqClassName, foldrName, buildName, eqStringName,
			  cCallableClassName, cReturnableClassName, 
			  enumClassName, ordClassName,
			  ratioDataConName, splitName, fstName, sndName,
			  ioDataConName, plusIntegerName, timesIntegerName,
			  replicatePName, mapPName, filterPName,
			  crossPName, zipPName, lengthPName, indexPName, toPName,
			  enumFromToPName, enumFromThenToPName, assertName,
			  fromIntegerName, fromRationalName, minusName, negateName,
			  qTyConName, monadNames )
import TysPrim		( charPrimTyCon, addrPrimTyCon, intPrimTyCon, 
			  floatPrimTyCon, doublePrimTyCon )
import TysWiredIn	( intTyCon )
import RdrName		( RdrName )
import Name		( Name, NamedThing(..), mkSystemName, nameSrcLoc, nameOccName )
import NameSet
import UnicodeUtil	( stringToUtf8 )
import UniqFM		( isNullUFM )
import UniqSet		( emptyUniqSet )
import List		( intersectBy )
import ListSetOps	( removeDups )
import Outputable
import FastString
\end{code}


*********************************************************
*							*
\subsection{Patterns}
*							*
*********************************************************

\begin{code}
rnPat :: RdrNamePat -> RnM (RenamedPat, FreeVars)

rnPat (WildPat _) = returnM (WildPat placeHolderType, emptyFVs)

rnPat (VarPat name)
  = lookupBndrRn  name			`thenM` \ vname ->
    returnM (VarPat vname, emptyFVs)

rnPat (SigPatIn pat ty)
  = doptM Opt_GlasgowExts `thenM` \ glaExts ->
    
    if glaExts
    then rnPat pat		`thenM` \ (pat', fvs1) ->
         rnHsTypeFVs doc ty	`thenM` \ (ty',  fvs2) ->
         returnM (SigPatIn pat' ty', fvs1 `plusFV` fvs2)

    else addErr (patSigErr ty)	`thenM_`
         rnPat pat
  where
    doc = text "In a pattern type-signature"
    
rnPat (LitPat s@(HsString _)) 
  = returnM (LitPat s, unitFV eqStringName)

rnPat (LitPat lit) 
  = litFVs lit		`thenM` \ fvs ->
    returnM (LitPat lit, fvs) 

rnPat (NPatIn lit mb_neg) 
  = rnOverLit lit			`thenM` \ (lit', fvs1) ->
    (case mb_neg of
	Nothing -> returnM (Nothing, emptyFVs)
	Just _  -> lookupSyntaxName negateName	`thenM` \ (neg, fvs) ->
		   returnM (Just neg, fvs)
    )					`thenM` \ (mb_neg', fvs2) ->
    returnM (NPatIn lit' mb_neg', 
	      fvs1 `plusFV` fvs2 `addOneFV` eqClassName)	
	-- Needed to find equality on pattern

rnPat (NPlusKPatIn name lit _)
  = rnOverLit lit			`thenM` \ (lit', fvs1) ->
    lookupBndrRn name			`thenM` \ name' ->
    lookupSyntaxName minusName		`thenM` \ (minus, fvs2) ->
    returnM (NPlusKPatIn name' lit' minus, 
	      fvs1 `plusFV` fvs2 `addOneFV` ordClassName)

rnPat (LazyPat pat)
  = rnPat pat		`thenM` \ (pat', fvs) ->
    returnM (LazyPat pat', fvs)

rnPat (AsPat name pat)
  = rnPat pat		`thenM` \ (pat', fvs) ->
    lookupBndrRn name	`thenM` \ vname ->
    returnM (AsPat vname pat', fvs)

rnPat (ConPatIn con stuff) = rnConPat con stuff


rnPat (ParPat pat)
  = rnPat pat		`thenM` \ (pat', fvs) ->
    returnM (ParPat pat', fvs)

rnPat (ListPat pats _)
  = mapFvRn rnPat pats			`thenM` \ (patslist, fvs) ->
    returnM (ListPat patslist placeHolderType, fvs `addOneFV` listTyCon_name)

rnPat (PArrPat pats _)
  = mapFvRn rnPat pats			`thenM` \ (patslist, fvs) ->
    returnM (PArrPat patslist placeHolderType, 
	      fvs `plusFV` implicit_fvs `addOneFV` parrTyCon_name)
  where
    implicit_fvs = mkFVs [lengthPName, indexPName]

rnPat (TuplePat pats boxed)
  = mapFvRn rnPat pats			`thenM` \ (patslist, fvs) ->
    returnM (TuplePat patslist boxed, fvs `addOneFV` tycon_name)
  where
    tycon_name = tupleTyCon_name boxed (length pats)

rnPat (TypePat name) =
    rnHsTypeFVs (text "In a type pattern") name	`thenM` \ (name', fvs) ->
    returnM (TypePat name', fvs)

------------------------------
rnConPat con (PrefixCon pats)
  = lookupOccRn con 	`thenM` \ con' ->
    mapFvRn rnPat pats	`thenM` \ (pats', fvs) ->
    returnM (ConPatIn con' (PrefixCon pats'), fvs `addOneFV` con')

rnConPat con (RecCon rpats)
  = lookupOccRn con 	`thenM` \ con' ->
    rnRpats rpats	`thenM` \ (rpats', fvs) ->
    returnM (ConPatIn con' (RecCon rpats'), fvs `addOneFV` con')

rnConPat con (InfixCon pat1 pat2)
  = lookupOccRn con 	`thenM` \ con' ->
    rnPat pat1		`thenM` \ (pat1', fvs1) ->
    rnPat pat2		`thenM` \ (pat2', fvs2) ->

    getModeRn		`thenM` \ mode ->
	-- See comments with rnExpr (OpApp ...)
    (if isInterfaceMode mode
	then returnM (ConPatIn con' (InfixCon pat1' pat2'))
	else lookupFixityRn con'	`thenM` \ fixity ->
	     mkConOpPatRn con' fixity pat1' pat2'
    )							`thenM` \ pat' ->
    returnM (pat', fvs1 `plusFV` fvs2 `addOneFV` con')
\end{code}


************************************************************************
*									*
\subsection{Match}
*									*
************************************************************************

\begin{code}
rnMatch :: HsMatchContext RdrName -> RdrNameMatch -> RnM (RenamedMatch, FreeVars)

rnMatch ctxt match@(Match pats maybe_rhs_sig grhss)
  = addSrcLoc (getMatchLoc match)	$

	-- Bind pattern-bound type variables
    let
	rhs_sig_tys =  case maybe_rhs_sig of
				Nothing -> []
				Just ty -> [ty]
	pat_sig_tys = collectSigTysFromPats pats
	doc_sig     = text "In a result type-signature"
 	doc_pat     = pprMatchContext ctxt
    in
    bindPatSigTyVars (rhs_sig_tys ++ pat_sig_tys)	$ 

	-- Note that we do a single bindLocalsRn for all the
	-- matches together, so that we spot the repeated variable in
	--	f x x = 1
    bindLocalsFVRn doc_pat (collectPatsBinders pats)	$ \ new_binders ->

    mapFvRn rnPat pats			`thenM` \ (pats', pat_fvs) ->
    rnGRHSs grhss			`thenM` \ (grhss', grhss_fvs) ->
    doptM Opt_GlasgowExts		`thenM` \ opt_GlasgowExts ->
    (case maybe_rhs_sig of
	Nothing -> returnM (Nothing, emptyFVs)
	Just ty | opt_GlasgowExts -> rnHsTypeFVs doc_sig ty	`thenM` \ (ty', ty_fvs) ->
				     returnM (Just ty', ty_fvs)
		| otherwise	  -> addErr (patSigErr ty)	`thenM_`
				     returnM (Nothing, emptyFVs)
    )					`thenM` \ (maybe_rhs_sig', ty_fvs) ->

    let
	binder_set     = mkNameSet new_binders
	unused_binders = nameSetToList (binder_set `minusNameSet` grhss_fvs)
	all_fvs	       = grhss_fvs `plusFV` pat_fvs `plusFV` ty_fvs
    in
    warnUnusedMatches unused_binders		`thenM_`
    
    returnM (Match pats' maybe_rhs_sig' grhss', all_fvs)
	-- The bindLocals and bindTyVars will remove the bound FVs
\end{code}


%************************************************************************
%*									*
\subsubsection{Guarded right-hand sides (GRHSs)}
%*									*
%************************************************************************

\begin{code}
rnGRHSs :: RdrNameGRHSs -> RnM (RenamedGRHSs, FreeVars)

rnGRHSs (GRHSs grhss binds _)
  = rnBinds binds		$ \ binds' ->
    mapFvRn rnGRHS grhss	`thenM` \ (grhss', fvGRHSs) ->
    returnM (GRHSs grhss' binds' placeHolderType, fvGRHSs)

rnGRHS (GRHS guarded locn)
  = doptM Opt_GlasgowExts		`thenM` \ opt_GlasgowExts ->
    addSrcLoc locn $		    
    (if not (opt_GlasgowExts || is_standard_guard guarded) then
		addWarn (nonStdGuardErr guarded)
     else
		returnM ()
    )		`thenM_`

    rnStmts guarded	`thenM` \ ((_, guarded'), fvs) ->
    returnM (GRHS guarded' locn, fvs)
  where
	-- Standard Haskell 1.4 guards are just a single boolean
	-- expression, rather than a list of qualifiers as in the
	-- Glasgow extension
    is_standard_guard [ResultStmt _ _]                 = True
    is_standard_guard [ExprStmt _ _ _, ResultStmt _ _] = True
    is_standard_guard other	      		       = False
\end{code}

%************************************************************************
%*									*
\subsubsection{Expressions}
%*									*
%************************************************************************

\begin{code}
rnExprs :: [RdrNameHsExpr] -> RnM ([RenamedHsExpr], FreeVars)
rnExprs ls = rnExprs' ls emptyUniqSet
 where
  rnExprs' [] acc = returnM ([], acc)
  rnExprs' (expr:exprs) acc
   = rnExpr expr 	        `thenM` \ (expr', fvExpr) ->

	-- Now we do a "seq" on the free vars because typically it's small
	-- or empty, especially in very long lists of constants
    let
	acc' = acc `plusFV` fvExpr
    in
    (grubby_seqNameSet acc' rnExprs') exprs acc'	`thenM` \ (exprs', fvExprs) ->
    returnM (expr':exprs', fvExprs)

-- Grubby little function to do "seq" on namesets; replace by proper seq when GHC can do seq
grubby_seqNameSet ns result | isNullUFM ns = result
			    | otherwise    = result
\end{code}

Variables. We look up the variable and return the resulting name. 

\begin{code}
rnExpr :: RdrNameHsExpr -> RnM (RenamedHsExpr, FreeVars)

rnExpr (HsVar v)
  = lookupOccRn v	`thenM` \ name ->
    if name `hasKey` assertIdKey then
	-- We expand it to (GHCerr.assert__ location)
        mkAssertExpr
    else
        -- The normal case
       returnM (HsVar name, unitFV name)

rnExpr (HsIPVar v)
  = newIPName v			`thenM` \ name ->
    let 
	fvs = case name of
		Linear _  -> mkFVs [splitName, fstName, sndName]
		Dupable _ -> emptyFVs 
    in   
    returnM (HsIPVar name, fvs)

rnExpr (HsLit lit) 
  = litFVs lit		`thenM` \ fvs -> 
    returnM (HsLit lit, fvs)

rnExpr (HsOverLit lit) 
  = rnOverLit lit		`thenM` \ (lit', fvs) ->
    returnM (HsOverLit lit', fvs)

rnExpr (HsLam match)
  = rnMatch LambdaExpr match	`thenM` \ (match', fvMatch) ->
    returnM (HsLam match', fvMatch)

rnExpr (HsApp fun arg)
  = rnExpr fun		`thenM` \ (fun',fvFun) ->
    rnExpr arg		`thenM` \ (arg',fvArg) ->
    returnM (HsApp fun' arg', fvFun `plusFV` fvArg)

rnExpr (OpApp e1 op _ e2) 
  = rnExpr e1				`thenM` \ (e1', fv_e1) ->
    rnExpr e2				`thenM` \ (e2', fv_e2) ->
    rnExpr op				`thenM` \ (op'@(HsVar op_name), fv_op) ->

	-- Deal with fixity
	-- When renaming code synthesised from "deriving" declarations
	-- we're in Interface mode, and we should ignore fixity; assume
	-- that the deriving code generator got the association correct
	-- Don't even look up the fixity when in interface mode
    getModeRn				`thenM` \ mode -> 
    (if isInterfaceMode mode
	then returnM (OpApp e1' op' defaultFixity e2')
	else lookupFixityRn op_name		`thenM` \ fixity ->
	     mkOpAppRn e1' op' fixity e2'
    )					`thenM` \ final_e -> 

    returnM (final_e,
	      fv_e1 `plusFV` fv_op `plusFV` fv_e2)

rnExpr (NegApp e _)
  = rnExpr e			`thenM` \ (e', fv_e) ->
    lookupSyntaxName negateName	`thenM` \ (neg_name, fv_neg) ->
    mkNegAppRn e' neg_name	`thenM` \ final_e ->
    returnM (final_e, fv_e `plusFV` fv_neg)

rnExpr (HsPar e)
  = rnExpr e 		`thenM` \ (e', fvs_e) ->
    returnM (HsPar e', fvs_e)

-- Template Haskell extensions
rnExpr (HsBracket br_body)
  = checkGHCI (thErr "bracket")		`thenM_`
    rnBracket br_body			`thenM` \ (body', fvs_e) ->
    returnM (HsBracket body', fvs_e `addOneFV` qTyConName)
	-- We use the Q tycon as a proxy to haul in all the smart
	-- constructors; see the hack in RnIfaces

rnExpr (HsSplice n e)
  = checkGHCI (thErr "splice")		`thenM_`
    getSrcLocM				`thenM` \ loc -> 
    newLocalsRn [(n,loc)]		`thenM` \ [n'] ->
    rnExpr e 				`thenM` \ (e', fvs_e) ->
    returnM (HsSplice n' e', fvs_e)    

rnExpr section@(SectionL expr op)
  = rnExpr expr	 				`thenM` \ (expr', fvs_expr) ->
    rnExpr op	 				`thenM` \ (op', fvs_op) ->
    checkSectionPrec InfixL section op' expr' `thenM_`
    returnM (SectionL expr' op', fvs_op `plusFV` fvs_expr)

rnExpr section@(SectionR op expr)
  = rnExpr op	 				`thenM` \ (op',   fvs_op) ->
    rnExpr expr	 				`thenM` \ (expr', fvs_expr) ->
    checkSectionPrec InfixR section op' expr'	`thenM_`
    returnM (SectionR op' expr', fvs_op `plusFV` fvs_expr)

rnExpr (HsCCall fun args may_gc is_casm _)
	-- Check out the comment on RnIfaces.getNonWiredDataDecl about ccalls
  = rnExprs args				`thenM` \ (args', fvs_args) ->
    returnM (HsCCall fun args' may_gc is_casm placeHolderType, 
	      fvs_args `plusFV` mkFVs [cCallableClassName, 
				       cReturnableClassName, 
				       ioDataConName])

rnExpr (HsSCC lbl expr)
  = rnExpr expr	 	`thenM` \ (expr', fvs_expr) ->
    returnM (HsSCC lbl expr', fvs_expr)

rnExpr (HsCase expr ms src_loc)
  = addSrcLoc src_loc $
    rnExpr expr		 		`thenM` \ (new_expr, e_fvs) ->
    mapFvRn (rnMatch CaseAlt) ms	`thenM` \ (new_ms, ms_fvs) ->
    returnM (HsCase new_expr new_ms src_loc, e_fvs `plusFV` ms_fvs)

rnExpr (HsLet binds expr)
  = rnBinds binds		$ \ binds' ->
    rnExpr expr			 `thenM` \ (expr',fvExpr) ->
    returnM (HsLet binds' expr', fvExpr)

rnExpr (HsWith expr binds is_with)
  = warnIf is_with withWarning `thenM_`
    rnExpr expr			`thenM` \ (expr',fvExpr) ->
    rnIPBinds binds		`thenM` \ (binds',fvBinds) ->
    returnM (HsWith expr' binds' is_with, fvExpr `plusFV` fvBinds)

rnExpr e@(HsDo do_or_lc stmts _ ty src_loc)
  = addSrcLoc src_loc $
    rnStmts stmts			`thenM` \ ((_, stmts'), fvs) ->

	-- Check the statement list ends in an expression
    case last stmts' of {
	ResultStmt _ _ -> returnM () ;
	_              -> addErr (doStmtListErr e)
    }					`thenM_`

	-- Generate the rebindable syntax for the monad
    (case do_or_lc of
	DoExpr -> mapAndUnzipM lookupSyntaxName monadNames
	other  -> returnM ([], [])
    )					`thenM` \ (monad_names', monad_fvs) ->

    returnM (HsDo do_or_lc stmts' monad_names' placeHolderType src_loc, 
	      fvs `plusFV` implicit_fvs `plusFV` plusFVs monad_fvs)
  where
    implicit_fvs = case do_or_lc of
      PArrComp -> mkFVs [replicatePName, mapPName, filterPName,
			 crossPName, zipPName]
      ListComp -> mkFVs [foldrName, buildName]
      DoExpr   -> emptyFVs

rnExpr (ExplicitList _ exps)
  = rnExprs exps		 	`thenM` \ (exps', fvs) ->
    returnM  (ExplicitList placeHolderType exps', fvs `addOneFV` listTyCon_name)

rnExpr (ExplicitPArr _ exps)
  = rnExprs exps		 	`thenM` \ (exps', fvs) ->
    returnM  (ExplicitPArr placeHolderType exps', 
	       fvs `addOneFV` toPName `addOneFV` parrTyCon_name)

rnExpr (ExplicitTuple exps boxity)
  = rnExprs exps	 			`thenM` \ (exps', fvs) ->
    returnM (ExplicitTuple exps' boxity, fvs `addOneFV` tycon_name)
  where
    tycon_name = tupleTyCon_name boxity (length exps)

rnExpr (RecordCon con_id rbinds)
  = lookupOccRn con_id 			`thenM` \ conname ->
    rnRbinds "construction" rbinds	`thenM` \ (rbinds', fvRbinds) ->
    returnM (RecordCon conname rbinds', fvRbinds `addOneFV` conname)

rnExpr (RecordUpd expr rbinds)
  = rnExpr expr			`thenM` \ (expr', fvExpr) ->
    rnRbinds "update" rbinds	`thenM` \ (rbinds', fvRbinds) ->
    returnM (RecordUpd expr' rbinds', fvExpr `plusFV` fvRbinds)

rnExpr (ExprWithTySig expr pty)
  = rnExpr expr			`thenM` \ (expr', fvExpr) ->
    rnHsTypeFVs doc pty		`thenM` \ (pty', fvTy) ->
    returnM (ExprWithTySig expr' pty', fvExpr `plusFV` fvTy)
  where 
    doc = text "In an expression type signature"

rnExpr (HsIf p b1 b2 src_loc)
  = addSrcLoc src_loc $
    rnExpr p		`thenM` \ (p', fvP) ->
    rnExpr b1		`thenM` \ (b1', fvB1) ->
    rnExpr b2		`thenM` \ (b2', fvB2) ->
    returnM (HsIf p' b1' b2' src_loc, plusFVs [fvP, fvB1, fvB2])

rnExpr (HsType a)
  = rnHsTypeFVs doc a	`thenM` \ (t, fvT) -> 
    returnM (HsType t, fvT)
  where 
    doc = text "In a type argument"

rnExpr (ArithSeqIn seq)
  = rn_seq seq	 			`thenM` \ (new_seq, fvs) ->
    returnM (ArithSeqIn new_seq, fvs `addOneFV` enumClassName)
  where
    rn_seq (From expr)
     = rnExpr expr 	`thenM` \ (expr', fvExpr) ->
       returnM (From expr', fvExpr)

    rn_seq (FromThen expr1 expr2)
     = rnExpr expr1 	`thenM` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenM` \ (expr2', fvExpr2) ->
       returnM (FromThen expr1' expr2', fvExpr1 `plusFV` fvExpr2)

    rn_seq (FromTo expr1 expr2)
     = rnExpr expr1	`thenM` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenM` \ (expr2', fvExpr2) ->
       returnM (FromTo expr1' expr2', fvExpr1 `plusFV` fvExpr2)

    rn_seq (FromThenTo expr1 expr2 expr3)
     = rnExpr expr1	`thenM` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenM` \ (expr2', fvExpr2) ->
       rnExpr expr3	`thenM` \ (expr3', fvExpr3) ->
       returnM (FromThenTo expr1' expr2' expr3',
		  plusFVs [fvExpr1, fvExpr2, fvExpr3])

rnExpr (PArrSeqIn seq)
  = rn_seq seq	 		       `thenM` \ (new_seq, fvs) ->
    returnM (PArrSeqIn new_seq, 
	      fvs `plusFV` mkFVs [enumFromToPName, enumFromThenToPName])
  where

    -- the parser shouldn't generate these two
    --
    rn_seq (From     _  ) = panic "RnExpr.rnExpr: Infinite parallel array!"
    rn_seq (FromThen _ _) = panic "RnExpr.rnExpr: Infinite parallel array!"

    rn_seq (FromTo expr1 expr2)
     = rnExpr expr1	`thenM` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenM` \ (expr2', fvExpr2) ->
       returnM (FromTo expr1' expr2', fvExpr1 `plusFV` fvExpr2)
    rn_seq (FromThenTo expr1 expr2 expr3)
     = rnExpr expr1	`thenM` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenM` \ (expr2', fvExpr2) ->
       rnExpr expr3	`thenM` \ (expr3', fvExpr3) ->
       returnM (FromThenTo expr1' expr2' expr3',
		  plusFVs [fvExpr1, fvExpr2, fvExpr3])
\end{code}

These three are pattern syntax appearing in expressions.
Since all the symbols are reservedops we can simply reject them.
We return a (bogus) EWildPat in each case.

\begin{code}
rnExpr e@EWildPat = addErr (patSynErr e)	`thenM_`
		    returnM (EWildPat, emptyFVs)

rnExpr e@(EAsPat _ _) = addErr (patSynErr e)	`thenM_`
		        returnM (EWildPat, emptyFVs)

rnExpr e@(ELazyPat _) = addErr (patSynErr e)	`thenM_`
		        returnM (EWildPat, emptyFVs)
\end{code}



%************************************************************************
%*									*
\subsubsection{@Rbinds@s and @Rpats@s: in record expressions}
%*									*
%************************************************************************

\begin{code}
rnRbinds str rbinds 
  = mappM_ field_dup_err dup_fields	`thenM_`
    mapFvRn rn_rbind rbinds		`thenM` \ (rbinds', fvRbind) ->
    returnM (rbinds', fvRbind)
  where
    (_, dup_fields) = removeDups compare [ f | (f,_) <- rbinds ]

    field_dup_err dups = addErr (dupFieldErr str dups)

    rn_rbind (field, expr)
      = lookupGlobalOccRn field	`thenM` \ fieldname ->
	rnExpr expr		`thenM` \ (expr', fvExpr) ->
	returnM ((fieldname, expr'), fvExpr `addOneFV` fieldname)

rnRpats rpats
  = mappM_ field_dup_err dup_fields 	`thenM_`
    mapFvRn rn_rpat rpats		`thenM` \ (rpats', fvs) ->
    returnM (rpats', fvs)
  where
    (_, dup_fields) = removeDups compare [ f | (f,_) <- rpats ]

    field_dup_err dups = addErr (dupFieldErr "pattern" dups)

    rn_rpat (field, pat)
      = lookupGlobalOccRn field	`thenM` \ fieldname ->
	rnPat pat		`thenM` \ (pat', fvs) ->
	returnM ((fieldname, pat'), fvs `addOneFV` fieldname)
\end{code}

%************************************************************************
%*									*
\subsubsection{@rnIPBinds@s: in implicit parameter bindings}		*
%*									*
%************************************************************************

\begin{code}
rnIPBinds [] = returnM ([], emptyFVs)
rnIPBinds ((n, expr) : binds)
  = newIPName n			`thenM` \ name ->
    rnExpr expr			`thenM` \ (expr',fvExpr) ->
    rnIPBinds binds		`thenM` \ (binds',fvBinds) ->
    returnM ((name, expr') : binds', fvExpr `plusFV` fvBinds)

\end{code}

%************************************************************************
%*									*
	Template Haskell brackets
%*									*
%************************************************************************

\begin{code}
rnBracket (ExpBr e) = rnExpr e		`thenM` \ (e', fvs) ->
		      returnM (ExpBr e', fvs)
rnBracket (PatBr p) = rnPat p		`thenM` \ (p', fvs) ->
		      returnM (PatBr p', fvs)
rnBracket (TypBr t) = rnHsTypeFVs doc t	`thenM` \ (t', fvs) ->
		      returnM (TypBr t', fvs)
		    where
		      doc = ptext SLIT("In a Template-Haskell quoted type")
rnBracket (DecBr ds) = rnSrcDecls ds	`thenM` \ (tcg_env, ds', fvs) ->
			-- Discard the tcg_env; it contains the extended global RdrEnv
			-- because there is no scope that these decls cover (yet!)
		       returnM (DecBr ds', fvs)
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
rnStmts :: [RdrNameStmt]
	-> RnM (([Name], [RenamedStmt]), FreeVars)

rnStmts []
  = returnM (([], []), emptyFVs)

rnStmts (stmt:stmts)
  = getLocalRdrEnv 		`thenM` \ name_env ->
    rnStmt stmt				$ \ stmt' ->
    rnStmts stmts			`thenM` \ ((binders, stmts'), fvs) ->
    returnM ((binders, stmt' : stmts'), fvs)

rnStmt :: RdrNameStmt
       -> (RenamedStmt -> RnM (([Name], a), FreeVars))
       -> RnM (([Name], a), FreeVars)
-- The thing list of names returned is the list returned by the
-- thing_inside, plus the binders of the arguments stmt

rnStmt (ParStmt stmtss) thing_inside
  = mapFvRn rnStmts stmtss		`thenM` \ (bndrstmtss, fv_stmtss) ->
    let binderss = map fst bndrstmtss
	checkBndrs all_bndrs bndrs
	  = checkErr (null (intersectBy eqOcc all_bndrs bndrs)) err `thenM_`
	    returnM (bndrs ++ all_bndrs)
	eqOcc n1 n2 = nameOccName n1 == nameOccName n2
	err = text "duplicate binding in parallel list comprehension"
    in
    foldlM checkBndrs [] binderss	`thenM` \ new_binders ->
    bindLocalNamesFV new_binders	$
    thing_inside (ParStmtOut bndrstmtss)`thenM` \ ((rest_bndrs, result), fv_rest) ->
    returnM ((new_binders ++ rest_bndrs, result), fv_stmtss `plusFV` fv_rest)

rnStmt (BindStmt pat expr src_loc) thing_inside
  = addSrcLoc src_loc $
    rnExpr expr					`thenM` \ (expr', fv_expr) ->
    bindPatSigTyVars (collectSigTysFromPat pat)	$ 
    bindLocalsFVRn doc (collectPatBinders pat)	$ \ new_binders ->
    rnPat pat					`thenM` \ (pat', fv_pat) ->
    thing_inside (BindStmt pat' expr' src_loc)	`thenM` \ ((rest_binders, result), fvs) ->
    returnM ((new_binders ++ rest_binders, result),
	      fv_expr `plusFV` fvs `plusFV` fv_pat)
  where
    doc = text "In a pattern in 'do' binding" 

rnStmt (ExprStmt expr _ src_loc) thing_inside
  = addSrcLoc src_loc $
    rnExpr expr 						`thenM` \ (expr', fv_expr) ->
    thing_inside (ExprStmt expr' placeHolderType src_loc)	`thenM` \ (result, fvs) ->
    returnM (result, fv_expr `plusFV` fvs)

rnStmt (ResultStmt expr src_loc) thing_inside
  = addSrcLoc src_loc $
    rnExpr expr 				`thenM` \ (expr', fv_expr) ->
    thing_inside (ResultStmt expr' src_loc)	`thenM` \ (result, fvs) ->
    returnM (result, fv_expr `plusFV` fvs)

rnStmt (LetStmt binds) thing_inside
  = rnBinds binds				$ \ binds' ->
    let new_binders = collectHsBinders binds' in
    thing_inside (LetStmt binds')    `thenM` \ ((rest_binders, result), fvs) ->
    returnM ((new_binders ++ rest_binders, result), fvs )
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
operator appications left-associatively, EXCEPT negation, which
we need to handle specially.

\begin{code}
mkOpAppRn :: RenamedHsExpr			-- Left operand; already rearranged
	  -> RenamedHsExpr -> Fixity 		-- Operator and fixity
	  -> RenamedHsExpr			-- Right operand (not an OpApp, but might
						-- be a NegApp)
	  -> RnM RenamedHsExpr

---------------------------
-- (e11 `op1` e12) `op2` e2
mkOpAppRn e1@(OpApp e11 op1 fix1 e12) op2 fix2 e2
  | nofix_error
  = addErr (precParseErr (ppr_op op1,fix1) (ppr_op op2,fix2))	`thenM_`
    returnM (OpApp e1 op2 fix2 e2)

  | associate_right
  = mkOpAppRn e12 op2 fix2 e2		`thenM` \ new_e ->
    returnM (OpApp e11 op1 fix1 new_e)
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2

---------------------------
--	(- neg_arg) `op` e2
mkOpAppRn e1@(NegApp neg_arg neg_name) op2 fix2 e2
  | nofix_error
  = addErr (precParseErr (pp_prefix_minus,negateFixity) (ppr_op op2,fix2))	`thenM_`
    returnM (OpApp e1 op2 fix2 e2)

  | associate_right
  = mkOpAppRn neg_arg op2 fix2 e2	`thenM` \ new_e ->
    returnM (NegApp new_e neg_name)
  where
    (nofix_error, associate_right) = compareFixity negateFixity fix2

---------------------------
--	e1 `op` - neg_arg
mkOpAppRn e1 op1 fix1 e2@(NegApp neg_arg _)	-- NegApp can occur on the right
  | not associate_right				-- We *want* right association
  = addErr (precParseErr (ppr_op op1, fix1) (pp_prefix_minus, negateFixity))	`thenM_`
    returnM (OpApp e1 op1 fix1 e2)
  where
    (_, associate_right) = compareFixity fix1 negateFixity

---------------------------
--	Default case
mkOpAppRn e1 op fix e2 			-- Default case, no rearrangment
  = ASSERT2( right_op_ok fix e2,
	     ppr e1 $$ text "---" $$ ppr op $$ text "---" $$ ppr fix $$ text "---" $$ ppr e2
    )
    returnM (OpApp e1 op fix e2)

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
mkNegAppRn neg_arg neg_name
  = 
#ifdef DEBUG
    getModeRn			`thenM` \ mode ->
    ASSERT( not_op_app mode neg_arg )
#endif
    returnM (NegApp neg_arg neg_name)

not_op_app SourceMode (OpApp _ _ _ _) = False
not_op_app mode other	 	      = True
\end{code}

\begin{code}
mkConOpPatRn :: Name -> Fixity -> RenamedPat -> RenamedPat
	     -> RnM RenamedPat

mkConOpPatRn op2 fix2 p1@(ConPatIn op1 (InfixCon p11 p12)) p2
  = lookupFixityRn op1		`thenM` \ fix1 ->
    let
	(nofix_error, associate_right) = compareFixity fix1 fix2
    in
    if nofix_error then
	addErr (precParseErr (ppr_op op1,fix1) (ppr_op op2,fix2))	`thenM_`
	returnM (ConPatIn op2 (InfixCon p1 p2))
    else 
    if associate_right then
	mkConOpPatRn op2 fix2 p12 p2		`thenM` \ new_p ->
	returnM (ConPatIn op1 (InfixCon p11 new_p))
    else
    returnM (ConPatIn op2 (InfixCon p1 p2))

mkConOpPatRn op fix p1 p2 			-- Default case, no rearrangment
  = ASSERT( not_op_pat p2 )
    returnM (ConPatIn op (InfixCon p1 p2))

not_op_pat (ConPatIn _ (InfixCon _ _)) = False
not_op_pat other   	               = True
\end{code}

\begin{code}
checkPrecMatch :: Bool -> Name -> RenamedMatch -> RnM ()

checkPrecMatch False fn match
  = returnM ()

checkPrecMatch True op (Match (p1:p2:_) _ _)
	-- True indicates an infix lhs
  = getModeRn 		`thenM` \ mode ->
	-- See comments with rnExpr (OpApp ...)
    if isInterfaceMode mode
	then returnM ()
	else checkPrec op p1 False	`thenM_`
	     checkPrec op p2 True

checkPrecMatch True op _ = panic "checkPrecMatch"

checkPrec op (ConPatIn op1 (InfixCon _ _)) right
  = lookupFixityRn op	`thenM` \  op_fix@(Fixity op_prec  op_dir) ->
    lookupFixityRn op1	`thenM` \ op1_fix@(Fixity op1_prec op1_dir) ->
    let
	inf_ok = op1_prec > op_prec || 
	         (op1_prec == op_prec &&
		  (op1_dir == InfixR && op_dir == InfixR && right ||
		   op1_dir == InfixL && op_dir == InfixL && not right))

	info  = (ppr_op op,  op_fix)
	info1 = (ppr_op op1, op1_fix)
	(infol, infor) = if right then (info, info1) else (info1, info)
    in
    checkErr inf_ok (precParseErr infol infor)

checkPrec op pat right
  = returnM ()

-- Check precedence of (arg op) or (op arg) respectively
-- If arg is itself an operator application, then either
--   (a) its precedence must be higher than that of op
--   (b) its precedency & associativity must be the same as that of op
checkSectionPrec direction section op arg
  = case arg of
	OpApp _ op fix _ -> go_for_it (ppr_op op)     fix
	NegApp _ _	 -> go_for_it pp_prefix_minus negateFixity
	other		 -> returnM ()
  where
    HsVar op_name = op
    go_for_it pp_arg_op arg_fix@(Fixity arg_prec assoc)
	= lookupFixityRn op_name	`thenM` \ op_fix@(Fixity op_prec _) ->
	  checkErr (op_prec < arg_prec
		     || op_prec == arg_prec && direction == assoc)
		  (sectionPrecErr (ppr_op op_name, op_fix) 	
		  (pp_arg_op, arg_fix) section)
\end{code}


%************************************************************************
%*									*
\subsubsection{Literals}
%*									*
%************************************************************************

When literals occur we have to make sure
that the types and classes they involve
are made available.

\begin{code}
litFVs (HsChar c)
   = checkErr (inCharRange c) (bogusCharError c) `thenM_`
     returnM (unitFV charTyCon_name)

litFVs (HsCharPrim c)         = returnM (unitFV (getName charPrimTyCon))
litFVs (HsString s)           = returnM (mkFVs [listTyCon_name, charTyCon_name])
litFVs (HsStringPrim s)       = returnM (unitFV (getName addrPrimTyCon))
litFVs (HsInt i)	      = returnM (unitFV (getName intTyCon))
litFVs (HsIntPrim i)          = returnM (unitFV (getName intPrimTyCon))
litFVs (HsFloatPrim f)        = returnM (unitFV (getName floatPrimTyCon))
litFVs (HsDoublePrim d)       = returnM (unitFV (getName doublePrimTyCon))
litFVs (HsLitLit l bogus_ty)  = returnM (unitFV cCallableClassName)
litFVs lit		      = pprPanic "RnExpr.litFVs" (ppr lit)	-- HsInteger and HsRat only appear 
									-- in post-typechecker translations

rnOverLit (HsIntegral i _)
  = lookupSyntaxName fromIntegerName	`thenM` \ (from_integer_name, fvs) ->
    if inIntRange i then
	returnM (HsIntegral i from_integer_name, fvs)
    else let
	extra_fvs = mkFVs [plusIntegerName, timesIntegerName]
	-- Big integer literals are built, using + and *, 
	-- out of small integers (DsUtils.mkIntegerLit)
	-- [NB: plusInteger, timesInteger aren't rebindable... 
	--	they are used to construct the argument to fromInteger, 
	--	which is the rebindable one.]
    in
    returnM (HsIntegral i from_integer_name, fvs `plusFV` extra_fvs)

rnOverLit (HsFractional i _)
  = lookupSyntaxName fromRationalName		`thenM` \ (from_rat_name, fvs) ->
    let
	extra_fvs = mkFVs [ratioDataConName, plusIntegerName, timesIntegerName]
	-- We have to make sure that the Ratio type is imported with
	-- its constructor, because literals of type Ratio t are
	-- built with that constructor.
	-- The Rational type is needed too, but that will come in
	-- as part of the type for fromRational.
	-- The plus/times integer operations may be needed to construct the numerator
	-- and denominator (see DsUtils.mkIntegerLit)
    in
    returnM (HsFractional i from_rat_name, fvs `plusFV` extra_fvs)
\end{code}

%************************************************************************
%*									*
\subsubsection{Assertion utils}
%*									*
%************************************************************************

\begin{code}
mkAssertExpr :: RnM (RenamedHsExpr, FreeVars)
mkAssertExpr
  = getSrcLocM    			`thenM` \ sloc ->

    -- if we're ignoring asserts, return (\ _ e -> e)
    -- if not, return (assertError "src-loc")

    if opt_IgnoreAsserts then
      newUnique				`thenM` \ uniq ->
      let
       vname = mkSystemName uniq FSLIT("v")
       expr  = HsLam ignorePredMatch
       loc   = nameSrcLoc vname
       ignorePredMatch = mkSimpleMatch [WildPat placeHolderType, VarPat vname] 
 				       (HsVar vname) placeHolderType loc
      in
      returnM (expr, emptyFVs)
    else
      let
        expr = 
          HsApp (HsVar assertName)
	        (HsLit (HsStringPrim (mkFastString (stringToUtf8 (showSDoc (ppr sloc))))))
      in
      returnM (expr, unitFV assertName)
\end{code}

%************************************************************************
%*									*
\subsubsection{Errors}
%*									*
%************************************************************************

\begin{code}
ppr_op op = quotes (ppr op)	-- Here, op can be a Name or a (Var n), where n is a Name
pp_prefix_minus = ptext SLIT("prefix `-'")

dupFieldErr str (dup:rest)
  = hsep [ptext SLIT("duplicate field name"), 
          quotes (ppr dup),
	  ptext SLIT("in record"), text str]

nonStdGuardErr guard
  = hang (ptext
    SLIT("accepting non-standard pattern guards (-fglasgow-exts to suppress this message)")
    ) 4 (ppr guard)

patSigErr ty
  =  (ptext SLIT("Illegal signature in pattern:") <+> ppr ty)
	$$ nest 4 (ptext SLIT("Use -fglasgow-exts to permit it"))

patSynErr e 
  = sep [ptext SLIT("Pattern syntax in expression context:"),
	 nest 4 (ppr e)]

thErr what
  = ptext SLIT("Template Haskell") <+> text what <+>  
    ptext SLIT("illegal in a stage-1 compiler") 

doStmtListErr e
  = sep [ptext SLIT("`do' statements must end in expression:"),
	 nest 4 (ppr e)]

bogusCharError c
  = ptext SLIT("character literal out of range: '\\") <> int c <> char '\''

withWarning
  = sep [quotes (ptext SLIT("with")),
	 ptext SLIT("is deprecated, use"),
	 quotes (ptext SLIT("let")),
	 ptext SLIT("instead")]
\end{code}
