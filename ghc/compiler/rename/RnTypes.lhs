%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnTypes (  rnHsType, rnHsSigType, rnHsTypeFVs, rnHsSigTypeFVs, 
		  rnContext, precParseErr, sectionPrecErr ) where

import CmdLineOpts	( DynFlag(Opt_WarnMisc, Opt_WarnUnusedMatches, Opt_GlasgowExts) )

import HsSyn
import RdrHsSyn	( RdrNameContext, RdrNameHsType, extractHsTyRdrTyVars, extractHsCtxtRdrTyVars )
import RnHsSyn	( RenamedContext, RenamedHsType, extractHsTyNames )
import RnEnv	( lookupOccRn, newIPName, bindTyVarsRn, lookupFixityRn )
import TcRnMonad

import PrelInfo	( cCallishClassKeys )
import RdrName	( elemRdrEnv )
import Name	( Name )
import NameSet	( FreeVars )
import Unique	( Uniquable(..) )

import BasicTypes	( compareFixity, arrowFixity )
import List		( nub )
import ListSetOps	( removeDupsEq )
import Outputable

#include "HsVersions.h"
\end{code}

These type renamers are in a separate module, rather than in (say) RnSource,
to break several loop.

%*********************************************************
%*							*
\subsection{Renaming types}
%*							*
%*********************************************************

\begin{code}
rnHsTypeFVs :: SDoc -> RdrNameHsType -> RnM (RenamedHsType, FreeVars)
rnHsTypeFVs doc_str ty 
  = rnHsType doc_str ty		`thenM` \ ty' ->
    returnM (ty', extractHsTyNames ty')

rnHsSigTypeFVs :: SDoc -> RdrNameHsType -> RnM (RenamedHsType, FreeVars)
rnHsSigTypeFVs doc_str ty
  = rnHsSigType doc_str ty	`thenM` \ ty' ->
    returnM (ty', extractHsTyNames ty')

rnHsSigType :: SDoc -> RdrNameHsType -> RnM RenamedHsType
	-- rnHsSigType is used for source-language type signatures,
	-- which use *implicit* universal quantification.
rnHsSigType doc_str ty
  = rnHsType (text "In the type signature for" <+> doc_str) ty
\end{code}

rnHsType is here because we call it from loadInstDecl, and I didn't
want a gratuitous knot.

\begin{code}
rnHsType :: SDoc -> RdrNameHsType -> RnM RenamedHsType

rnHsType doc (HsForAllTy Nothing ctxt ty)
	-- Implicit quantifiction in source code (no kinds on tyvars)
	-- Given the signature  C => T  we universally quantify 
	-- over FV(T) \ {in-scope-tyvars} 
  = getLocalRdrEnv		`thenM` \ name_env ->
    let
	mentioned_in_tau  = extractHsTyRdrTyVars ty
	mentioned_in_ctxt = extractHsCtxtRdrTyVars ctxt
	mentioned	  = nub (mentioned_in_tau ++ mentioned_in_ctxt)

	-- Don't quantify over type variables that are in scope;
	-- when GlasgowExts is off, there usually won't be any, except for
	-- class signatures:
	--	class C a where { op :: a -> a }
	forall_tyvars = filter (not . (`elemRdrEnv` name_env)) mentioned
    in
    rnForAll doc (map UserTyVar forall_tyvars) ctxt ty

rnHsType doc (HsForAllTy (Just forall_tyvars) ctxt tau)
	-- Explicit quantification.
	-- Check that the forall'd tyvars are actually 
	-- mentioned in the type, and produce a warning if not
  = let
	mentioned_in_tau		= extractHsTyRdrTyVars tau
	mentioned_in_ctxt		= extractHsCtxtRdrTyVars ctxt
	mentioned			= nub (mentioned_in_tau ++ mentioned_in_ctxt)
	forall_tyvar_names		= hsTyVarNames forall_tyvars

	-- Explicitly quantified but not mentioned in ctxt or tau
	warn_guys			= filter (`notElem` mentioned) forall_tyvar_names
    in
    mappM_ (forAllWarn doc tau) warn_guys	`thenM_`
    rnForAll doc forall_tyvars ctxt tau

rnHsType doc (HsTyVar tyvar)
  = lookupOccRn tyvar 		`thenM` \ tyvar' ->
    returnM (HsTyVar tyvar')

rnHsType doc (HsOpTy ty1 op ty2)
  = (case op of
	HsArrow  -> returnM HsArrow
	HsTyOp n -> lookupOccRn n    `thenM` \ n' ->
		    returnM (HsTyOp n')
    )				`thenM` \ op' ->
    rnHsType doc ty1		`thenM` \ ty1' ->
    rnHsType doc ty2		`thenM` \ ty2' -> 
    lookupTyFixityRn op'	`thenM` \ fix ->
    mkHsOpTyRn op' fix ty1' ty2'

rnHsType doc (HsParTy ty)
  = rnHsType doc ty 	        `thenM` \ ty' ->
    returnM (HsParTy ty')

rnHsType doc (HsNumTy i)
  | i == 1    = returnM (HsNumTy i)
  | otherwise = addErr err_msg	`thenM_`  returnM (HsNumTy i)
  where
    err_msg = ptext SLIT("Only unit numeric type pattern is valid")
			   

rnHsType doc (HsFunTy ty1 ty2)
  = rnHsType doc ty1	`thenM` \ ty1' ->
	-- Might find a for-all as the arg of a function type
    rnHsType doc ty2	`thenM` \ ty2' ->
	-- Or as the result.  This happens when reading Prelude.hi
	-- when we find return :: forall m. Monad m -> forall a. a -> m a
    returnM (HsFunTy ty1' ty2')

rnHsType doc (HsListTy ty)
  = rnHsType doc ty				`thenM` \ ty' ->
    returnM (HsListTy ty')

rnHsType doc (HsKindSig ty k)
  = rnHsType doc ty				`thenM` \ ty' ->
    returnM (HsKindSig ty' k)

rnHsType doc (HsPArrTy ty)
  = rnHsType doc ty				`thenM` \ ty' ->
    returnM (HsPArrTy ty')

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsType doc (HsTupleTy tup_con tys)
  = mappM (rnHsType doc) tys	  	`thenM` \ tys' ->
    returnM (HsTupleTy tup_con tys')

rnHsType doc (HsAppTy ty1 ty2)
  = rnHsType doc ty1		`thenM` \ ty1' ->
    rnHsType doc ty2		`thenM` \ ty2' ->
    returnM (HsAppTy ty1' ty2')

rnHsType doc (HsPredTy pred)
  = rnPred doc pred	`thenM` \ pred' ->
    returnM (HsPredTy pred')

rnHsTypes doc tys = mappM (rnHsType doc) tys
\end{code}


\begin{code}
rnForAll doc forall_tyvars ctxt ty
  = bindTyVarsRn doc forall_tyvars	$ \ new_tyvars ->
    rnContext doc ctxt			`thenM` \ new_ctxt ->
    rnHsType doc ty			`thenM` \ new_ty ->
    returnM (mkHsForAllTy (Just new_tyvars) new_ctxt new_ty)
\end{code}


%*********************************************************
%*							*
\subsection{Fixities}
%*							*
%*********************************************************

Infix types are read in a *right-associative* way, so that
	a `op` b `op` c
is always read in as
	a `op` (b `op` c)

mkHsOpTyRn rearranges where necessary.  The two arguments
have already been renamed and rearranged.  It's made rather tiresome
by the presence of ->

\begin{code}
lookupTyFixityRn HsArrow    = returnM arrowFixity
lookupTyFixityRn (HsTyOp n) 
  = doptM Opt_GlasgowExts 			`thenM` \ glaExts ->
    warnIf (not glaExts) (infixTyConWarn n)	`thenM_`
    lookupFixityRn n

-- Building (ty1 `op1` (ty21 `op2` ty22))
mkHsOpTyRn :: HsTyOp Name -> Fixity 
	   -> RenamedHsType -> RenamedHsType 
	   -> RnM RenamedHsType

mkHsOpTyRn op1 fix1 ty1 ty2@(HsOpTy ty21 op2 ty22)
  = lookupTyFixityRn op2		`thenM` \ fix2 ->
    let
	(nofix_error, associate_right) = compareFixity fix1 fix2
    in
    if nofix_error then
	addErr (precParseErr (quotes (ppr op1),fix1) 
			       (quotes (ppr op2),fix2))	`thenM_`
	returnM (HsOpTy ty1 op1 ty2)
    else 
    if not associate_right then
	-- Rearrange to ((ty1 `op1` ty21) `op2` ty22)
	mkHsOpTyRn op1 fix1 ty1 ty21		`thenM` \ new_ty ->
	returnM (HsOpTy new_ty op2 ty22)
    else
    returnM (HsOpTy ty1 op1 ty2)

mkHsOpTyRn op fix ty1 ty2 			-- Default case, no rearrangment
  = returnM (HsOpTy ty1 op ty2)

mkHsFunTyRn ty1 ty2			-- Precedence of function arrow is 0
  = returnM (HsFunTy ty1 ty2)		-- so no rearrangement reqd.  Change
					-- this if fixity of -> increases.

not_op_ty (HsOpTy _ _ _) = False
not_op_ty other   	 = True
\end{code}

%*********************************************************
%*							*
\subsection{Contexts and predicates}
%*							*
%*********************************************************

\begin{code}
rnContext :: SDoc -> RdrNameContext -> RnM RenamedContext
rnContext doc ctxt
  = mappM rn_pred ctxt		`thenM` \ theta ->

	-- Check for duplicate assertions
	-- If this isn't an error, then it ought to be:
    ifOptM Opt_WarnMisc (
        let
	    (_, dups) = removeDupsEq theta
		-- We only have equality, not ordering
        in
        mappM_ (addWarn . dupClassAssertWarn theta) dups
    )				`thenM_`

    returnM theta
  where
   	--Someone discovered that @CCallable@ and @CReturnable@
	-- could be used in contexts such as:
	--	foo :: CCallable a => a -> PrimIO Int
	-- Doing this utterly wrecks the whole point of introducing these
	-- classes so we specifically check that this isn't being done.
    rn_pred pred = rnPred doc pred				`thenM` \ pred'->
		   checkErr (not (bad_pred pred'))
			    (naughtyCCallContextErr pred')	`thenM_`
		   returnM pred'

    bad_pred (HsClassP clas _) = getUnique clas `elem` cCallishClassKeys
    bad_pred other	       = False


rnPred doc (HsClassP clas tys)
  = lookupOccRn clas		`thenM` \ clas_name ->
    rnHsTypes doc tys		`thenM` \ tys' ->
    returnM (HsClassP clas_name tys')

rnPred doc (HsIParam n ty)
  = newIPName n			`thenM` \ name ->
    rnHsType doc ty		`thenM` \ ty' ->
    returnM (HsIParam name ty')
\end{code}


%*********************************************************
%*							*
\subsection{Errors}
%*							*
%*********************************************************

\end{code}
\begin{code}
forAllWarn doc ty tyvar
  = ifOptM Opt_WarnUnusedMatches 	$
    getModeRn				`thenM` \ mode ->
    case mode of {
#ifndef DEBUG
	     InterfaceMode _ -> returnM () ; -- Don't warn of unused tyvars in interface files
		                            -- unless DEBUG is on, in which case it is slightly
					    -- informative.  They can arise from mkRhsTyLam,
#endif					    -- leading to (say) 	f :: forall a b. [b] -> [b]
	     other ->
		addWarn (
		   sep [ptext SLIT("The universally quantified type variable") <+> quotes (ppr tyvar),
		   nest 4 (ptext SLIT("does not appear in the type") <+> quotes (ppr ty))]
		   $$
		   doc
                )
          }

dupClassAssertWarn ctxt (assertion : dups)
  = sep [hsep [ptext SLIT("Duplicate class assertion"), 
	       quotes (ppr assertion),
	       ptext SLIT("in the context:")],
	 nest 4 (pprHsContext ctxt <+> ptext SLIT("..."))]

naughtyCCallContextErr (HsClassP clas _)
  = sep [ptext SLIT("Can't use class") <+> quotes (ppr clas), 
	 ptext SLIT("in a context")]

precParseErr op1 op2 
  = hang (ptext SLIT("precedence parsing error"))
      4 (hsep [ptext SLIT("cannot mix"), ppr_opfix op1, ptext SLIT("and"), 
	       ppr_opfix op2,
	       ptext SLIT("in the same infix expression")])

sectionPrecErr op arg_op section
 = vcat [ptext SLIT("The operator") <+> ppr_opfix op <+> ptext SLIT("of a section"),
	 nest 4 (ptext SLIT("must have lower precedence than the operand") <+> ppr_opfix arg_op),
	 nest 4 (ptext SLIT("in the section:") <+> quotes (ppr section))]

infixTyConWarn op
  = ftext FSLIT("Accepting non-standard infix type constructor") <+> quotes (ppr op)

ppr_opfix (pp_op, fixity) = pp_op <+> brackets (ppr fixity)
\end{code}