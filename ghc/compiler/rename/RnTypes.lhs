%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnTypes (  rnHsType, rnHsSigType, rnHsTypeFVs, rnHsSigTypeFVs, rnContext ) where

import CmdLineOpts	( DynFlag(Opt_WarnMisc, Opt_WarnUnusedMatches) )

import HsSyn
import RdrHsSyn	( RdrNameContext, RdrNameHsType, extractHsTyRdrTyVars, extractHsCtxtRdrTyVars )
import RnHsSyn	( RenamedContext, RenamedHsType, extractHsTyNames, tupleTyCon_name )
import RnEnv	( lookupOccRn, newIPName, bindTyVarsRn )
import RnMonad

import PrelInfo	( cCallishClassKeys )
import RdrName	( elemRdrEnv )
import NameSet	( FreeVars )
import Unique	( Uniquable(..) )

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
rnHsTypeFVs :: SDoc -> RdrNameHsType -> RnMS (RenamedHsType, FreeVars)
rnHsTypeFVs doc_str ty 
  = rnHsType doc_str ty		`thenRn` \ ty' ->
    returnRn (ty', extractHsTyNames ty')

rnHsSigTypeFVs :: SDoc -> RdrNameHsType -> RnMS (RenamedHsType, FreeVars)
rnHsSigTypeFVs doc_str ty
  = rnHsSigType doc_str ty	`thenRn` \ ty' ->
    returnRn (ty', extractHsTyNames ty')

rnHsSigType :: SDoc -> RdrNameHsType -> RnMS RenamedHsType
	-- rnHsSigType is used for source-language type signatures,
	-- which use *implicit* universal quantification.
rnHsSigType doc_str ty
  = rnHsType (text "In the type signature for" <+> doc_str) ty
\end{code}

rnHsType is here because we call it from loadInstDecl, and I didn't
want a gratuitous knot.

\begin{code}
rnHsType :: SDoc -> RdrNameHsType -> RnMS RenamedHsType

rnHsType doc (HsForAllTy Nothing ctxt ty)
	-- Implicit quantifiction in source code (no kinds on tyvars)
	-- Given the signature  C => T  we universally quantify 
	-- over FV(T) \ {in-scope-tyvars} 
  = getLocalNameEnv		`thenRn` \ name_env ->
    let
	mentioned_in_tau  = extractHsTyRdrTyVars ty
	mentioned_in_ctxt = extractHsCtxtRdrTyVars ctxt
	mentioned	  = nub (mentioned_in_tau ++ mentioned_in_ctxt)
	forall_tyvars	  = filter (not . (`elemRdrEnv` name_env)) mentioned
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
    mapRn_ (forAllWarn doc tau) warn_guys	`thenRn_`
    rnForAll doc forall_tyvars ctxt tau

rnHsType doc (HsTyVar tyvar)
  = lookupOccRn tyvar 		`thenRn` \ tyvar' ->
    returnRn (HsTyVar tyvar')

rnHsType doc (HsOpTy ty1 opname ty2)
  = lookupOccRn opname	`thenRn` \ name' ->
    rnHsType doc ty1	`thenRn` \ ty1' ->
    rnHsType doc ty2	`thenRn` \ ty2' -> 
    returnRn (HsOpTy ty1' name' ty2')

rnHsType doc (HsNumTy i)
  | i == 1    = returnRn (HsNumTy i)
  | otherwise = failWithRn (HsNumTy i)
			   (ptext SLIT("Only unit numeric type pattern is valid"))

rnHsType doc (HsFunTy ty1 ty2)
  = rnHsType doc ty1	`thenRn` \ ty1' ->
	-- Might find a for-all as the arg of a function type
    rnHsType doc ty2	`thenRn` \ ty2' ->
	-- Or as the result.  This happens when reading Prelude.hi
	-- when we find return :: forall m. Monad m -> forall a. a -> m a
    returnRn (HsFunTy ty1' ty2')

rnHsType doc (HsListTy ty)
  = rnHsType doc ty				`thenRn` \ ty' ->
    returnRn (HsListTy ty')

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsType doc (HsTupleTy (HsTupCon _ boxity arity) tys)
	-- Don't do lookupOccRn, because this is built-in syntax
	-- so it doesn't need to be in scope
  = mapRn (rnHsType doc) tys	  	`thenRn` \ tys' ->
    returnRn (HsTupleTy (HsTupCon tup_name boxity arity) tys')
  where
    tup_name = tupleTyCon_name boxity arity
  

rnHsType doc (HsAppTy ty1 ty2)
  = rnHsType doc ty1		`thenRn` \ ty1' ->
    rnHsType doc ty2		`thenRn` \ ty2' ->
    returnRn (HsAppTy ty1' ty2')

rnHsType doc (HsPredTy pred)
  = rnPred doc pred	`thenRn` \ pred' ->
    returnRn (HsPredTy pred')

rnHsTypes doc tys = mapRn (rnHsType doc) tys
\end{code}

\begin{code}
rnForAll doc forall_tyvars ctxt ty
  = bindTyVarsRn doc forall_tyvars	$ \ new_tyvars ->
    rnContext doc ctxt			`thenRn` \ new_ctxt ->
    rnHsType doc ty			`thenRn` \ new_ty ->
    returnRn (mkHsForAllTy (Just new_tyvars) new_ctxt new_ty)
\end{code}

\begin{code}
rnContext :: SDoc -> RdrNameContext -> RnMS RenamedContext
rnContext doc ctxt
  = mapRn rn_pred ctxt		`thenRn` \ theta ->

	-- Check for duplicate assertions
	-- If this isn't an error, then it ought to be:
    ifOptRn Opt_WarnMisc (
        let
	    (_, dups) = removeDupsEq theta
		-- We only have equality, not ordering
        in
        mapRn (addWarnRn . dupClassAssertWarn theta) dups
    )				`thenRn_`

    returnRn theta
  where
   	--Someone discovered that @CCallable@ and @CReturnable@
	-- could be used in contexts such as:
	--	foo :: CCallable a => a -> PrimIO Int
	-- Doing this utterly wrecks the whole point of introducing these
	-- classes so we specifically check that this isn't being done.
    rn_pred pred = rnPred doc pred				`thenRn` \ pred'->
		   checkRn (not (bad_pred pred'))
			   (naughtyCCallContextErr pred')	`thenRn_`
		   returnRn pred'

    bad_pred (HsClassP clas _) = getUnique clas `elem` cCallishClassKeys
    bad_pred other	       = False


rnPred doc (HsClassP clas tys)
  = lookupOccRn clas		`thenRn` \ clas_name ->
    rnHsTypes doc tys		`thenRn` \ tys' ->
    returnRn (HsClassP clas_name tys')

rnPred doc (HsIParam n ty)
  = newIPName n			`thenRn` \ name ->
    rnHsType doc ty		`thenRn` \ ty' ->
    returnRn (HsIParam name ty')
\end{code}

\end{code}
\begin{code}
forAllWarn doc ty tyvar
  = ifOptRn Opt_WarnUnusedMatches 	$
    getModeRn				`thenRn` \ mode ->
    case mode of {
#ifndef DEBUG
	     InterfaceMode -> returnRn () ; -- Don't warn of unused tyvars in interface files
		                            -- unless DEBUG is on, in which case it is slightly
					    -- informative.  They can arise from mkRhsTyLam,
#endif					    -- leading to (say) 	f :: forall a b. [b] -> [b]
	     other ->
		addWarnRn (
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
\end{code}