%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RnHsSyn]{Specialisations of the @HsSyn@ syntax for the renamer}

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module RnHsSyn(
        -- Names
        charTyCon_name, listTyCon_name, parrTyCon_name, tupleTyCon_name,
        extractHsTyVars, extractHsTyNames, extractHsTyNames_s,
        extractFunDepNames, extractHsCtxtTyNames,
        extractHsTyVarBndrNames, extractHsTyVarBndrNames_s,

        -- Free variables
        hsSigsFVs, hsSigFVs, conDeclFVs, bangTyFVs
  ) where

#include "HsVersions.h"

import HsSyn
import Class            ( FunDep )
import TysWiredIn       ( tupleTyCon, listTyCon, parrTyCon, charTyCon )
import Name             ( Name, getName, isTyVarName )
import NameSet
import BasicTypes       ( TupleSort )
import SrcLoc
import Panic            ( panic )
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Free variables}
%*                                                                      *
%************************************************************************

These free-variable finders returns tycons and classes too.

\begin{code}
charTyCon_name, listTyCon_name, parrTyCon_name :: Name
charTyCon_name    = getName charTyCon
listTyCon_name    = getName listTyCon
parrTyCon_name    = getName parrTyCon

tupleTyCon_name :: TupleSort -> Int -> Name
tupleTyCon_name sort n = getName (tupleTyCon sort n)

extractHsTyVars :: LHsType Name -> NameSet
extractHsTyVars x = filterNameSet isTyVarName (extractHsTyNames x)

extractFunDepNames :: FunDep Name -> NameSet
extractFunDepNames (ns1, ns2) = mkNameSet ns1 `unionNameSets` mkNameSet ns2

extractHsTyNames   :: LHsType Name -> NameSet
-- Also extract names in kinds.
extractHsTyNames ty
  = getl ty
  where
    getl (L _ ty) = get ty

    get (HsAppTy ty1 ty2)      = getl ty1 `unionNameSets` getl ty2
    get (HsListTy ty)          = unitNameSet listTyCon_name `unionNameSets` getl ty
    get (HsPArrTy ty)          = unitNameSet parrTyCon_name `unionNameSets` getl ty
    get (HsTupleTy _ tys)      = extractHsTyNames_s tys
    get (HsFunTy ty1 ty2)      = getl ty1 `unionNameSets` getl ty2
    get (HsIParamTy _ ty)      = getl ty
    get (HsEqTy ty1 ty2)       = getl ty1 `unionNameSets` getl ty2
    get (HsOpTy ty1 (_, op) ty2) = getl ty1 `unionNameSets` getl ty2 `unionNameSets` unitNameSet (unLoc op)
    get (HsParTy ty)           = getl ty
    get (HsBangTy _ ty)        = getl ty
    get (HsRecTy flds)         = extractHsTyNames_s (map cd_fld_type flds)
    get (HsTyVar tv)           = unitNameSet tv
    get (HsSpliceTy _ fvs _)   = fvs
    get (HsQuasiQuoteTy {})    = emptyNameSet
    get (HsKindSig ty ki)      = getl ty `unionNameSets` getl ki
    get (HsForAllTy _ tvs
                    ctxt ty)   = extractHsTyVarBndrNames_s tvs
                                 (extractHsCtxtTyNames ctxt
                                  `unionNameSets` getl ty)
    get (HsDocTy ty _)         = getl ty
    get (HsCoreTy {})          = emptyNameSet	-- This probably isn't quite right
    		  	       	 		-- but I don't think it matters
    get (HsExplicitListTy _ tys) = extractHsTyNames_s tys
    get (HsExplicitTupleTy _ tys) = extractHsTyNames_s tys
    get (HsWrapTy {})          = panic "extractHsTyNames"

extractHsTyNames_s  :: [LHsType Name] -> NameSet
extractHsTyNames_s tys = foldr (unionNameSets . extractHsTyNames) emptyNameSet tys

extractHsCtxtTyNames :: LHsContext Name -> NameSet
extractHsCtxtTyNames (L _ ctxt)
  = foldr (unionNameSets . extractHsTyNames) emptyNameSet ctxt

extractHsTyVarBndrNames :: LHsTyVarBndr Name -> NameSet
extractHsTyVarBndrNames (L _ (UserTyVar _ _)) = emptyNameSet
extractHsTyVarBndrNames (L _ (KindedTyVar _ ki _)) = extractHsTyNames ki

extractHsTyVarBndrNames_s :: [LHsTyVarBndr Name] -> NameSet -> NameSet
-- Update the name set 'body' by adding the names in the binders
-- kinds and handling scoping.
extractHsTyVarBndrNames_s [] body = body
extractHsTyVarBndrNames_s (b:bs) body =
  (extractHsTyVarBndrNames_s bs body `delFromNameSet` hsTyVarName (unLoc b))
  `unionNameSets` extractHsTyVarBndrNames b
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Free variables of declarations}
%*                                                                      *
%************************************************************************

Return the Names that must be in scope if we are to use this declaration.
In all cases this is set up for interface-file declarations:
        - for class decls we ignore the bindings
        - for instance decls likewise, plus the pragmas
        - for rule decls, we ignore HsRules
        - for data decls, we ignore derivings

        *** See "THE NAMING STORY" in HsDecls ****

\begin{code}
----------------
hsSigsFVs :: [LSig Name] -> FreeVars
hsSigsFVs sigs = plusFVs (map (hsSigFVs.unLoc) sigs)

hsSigFVs :: Sig Name -> FreeVars
hsSigFVs (TypeSig _ ty)    = extractHsTyNames ty
hsSigFVs (GenericSig _ ty) = extractHsTyNames ty
hsSigFVs (SpecInstSig ty)  = extractHsTyNames ty
hsSigFVs (SpecSig _ ty _)  = extractHsTyNames ty
hsSigFVs _                 = emptyFVs

----------------
conDeclFVs :: LConDecl Name -> FreeVars
conDeclFVs (L _ (ConDecl { con_qvars = tyvars, con_cxt = context,
                           con_details = details, con_res = res_ty}))
  = extractHsTyVarBndrNames_s tyvars $
    extractHsCtxtTyNames context  `plusFV`
    conDetailsFVs details         `plusFV`
    conResTyFVs res_ty

conResTyFVs :: ResType Name -> FreeVars
conResTyFVs ResTyH98       = emptyFVs
conResTyFVs (ResTyGADT ty) = extractHsTyNames ty

conDetailsFVs :: HsConDeclDetails Name -> FreeVars
conDetailsFVs details = plusFVs (map bangTyFVs (hsConDeclArgTys details))

bangTyFVs :: LHsType Name -> FreeVars
bangTyFVs bty = extractHsTyNames (getBangType bty)
\end{code}
