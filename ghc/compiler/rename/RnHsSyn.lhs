%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RnHsSyn]{Specialisations of the @HsSyn@ syntax for the renamer}

\begin{code}
module RnHsSyn where

#include "HsVersions.h"

import HsSyn
import Class		( FunDep )
import TysWiredIn	( tupleTyCon, listTyCon, parrTyCon, charTyCon )
import Name		( Name, getName, isTyVarName )
import NameSet
import BasicTypes	( Boxity )
import Outputable
\end{code}


\begin{code}
type RenamedHsDecl		= HsDecl		Name
type RenamedArithSeqInfo	= ArithSeqInfo		Name
type RenamedClassOpSig		= Sig			Name
type RenamedConDecl		= ConDecl		Name
type RenamedContext		= HsContext 		Name
type RenamedRuleDecl		= RuleDecl		Name
type RenamedTyClDecl		= TyClDecl		Name
type RenamedDefaultDecl		= DefaultDecl		Name
type RenamedForeignDecl		= ForeignDecl		Name
type RenamedGRHS		= GRHS			Name
type RenamedGRHSs		= GRHSs			Name
type RenamedHsBinds		= HsBinds		Name
type RenamedHsExpr		= HsExpr		Name
type RenamedInstDecl		= InstDecl		Name
type RenamedMatchContext	= HsMatchContext	Name
type RenamedMatch		= Match			Name
type RenamedMonoBinds		= MonoBinds		Name
type RenamedPat			= InPat			Name
type RenamedHsType		= HsType		Name
type RenamedHsPred		= HsPred		Name
type RenamedRecordBinds		= HsRecordBinds		Name
type RenamedSig			= Sig			Name
type RenamedStmt		= Stmt			Name
type RenamedFixitySig		= FixitySig		Name
type RenamedDeprecation		= DeprecDecl		Name
type RenamedHsCmd		= HsCmd			Name
type RenamedHsCmdTop		= HsCmdTop		Name
\end{code}

%************************************************************************
%*									*
\subsection{Free variables}
%*									*
%************************************************************************

These free-variable finders returns tycons and classes too.

\begin{code}
charTyCon_name, listTyCon_name, parrTyCon_name :: Name
charTyCon_name    = getName charTyCon
listTyCon_name    = getName listTyCon
parrTyCon_name    = getName parrTyCon

tupleTyCon_name :: Boxity -> Int -> Name
tupleTyCon_name boxity n = getName (tupleTyCon boxity n)

extractHsTyVars :: RenamedHsType -> NameSet
extractHsTyVars x = filterNameSet isTyVarName (extractHsTyNames x)

extractFunDepNames :: FunDep Name -> NameSet
extractFunDepNames (ns1, ns2) = mkNameSet ns1 `unionNameSets` mkNameSet ns2

extractHsTyNames   :: RenamedHsType -> NameSet
extractHsTyNames ty
  = get ty
  where
    get (HsAppTy ty1 ty2)      = get ty1 `unionNameSets` get ty2
    get (HsListTy ty)          = unitNameSet listTyCon_name `unionNameSets` get ty
    get (HsPArrTy ty)          = unitNameSet parrTyCon_name `unionNameSets` get ty
    get (HsTupleTy con tys)    = extractHsTyNames_s tys
    get (HsFunTy ty1 ty2)      = get ty1 `unionNameSets` get ty2
    get (HsPredTy p)	       = extractHsPredTyNames p
    get (HsOpTy ty1 op ty2)    = get ty1 `unionNameSets` get ty2 `unionNameSets` unitNameSet op
    get (HsParTy ty)           = get ty
    get (HsNumTy n)            = emptyNameSet
    get (HsTyVar tv)	       = unitNameSet tv
    get (HsKindSig ty k)       = get ty
    get (HsForAllTy (Just tvs) 
		    ctxt ty)   = (extractHsCtxtTyNames ctxt `unionNameSets` get ty)
					    `minusNameSet`
				  mkNameSet (hsTyVarNames tvs)
    get ty@(HsForAllTy Nothing _ _) = pprPanic "extractHsTyNames" (ppr ty)

extractHsTyNames_s  :: [RenamedHsType] -> NameSet
extractHsTyNames_s tys = foldr (unionNameSets . extractHsTyNames) emptyNameSet tys

extractHsCtxtTyNames :: RenamedContext -> NameSet
extractHsCtxtTyNames ctxt = foldr (unionNameSets . extractHsPredTyNames) emptyNameSet ctxt

-- You don't import or export implicit parameters,
-- so don't mention the IP names
extractHsPredTyNames (HsClassP cls tys)
  = unitNameSet cls `unionNameSets` extractHsTyNames_s tys
extractHsPredTyNames (HsIParam n ty)
  = extractHsTyNames ty
\end{code}


%************************************************************************
%*									*
\subsection{Free variables of declarations}
%*									*
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
hsSigsFVs sigs = plusFVs (map hsSigFVs sigs)

hsSigFVs (Sig v ty _) 	    = extractHsTyNames ty
hsSigFVs (SpecInstSig ty _) = extractHsTyNames ty
hsSigFVs (SpecSig v ty _)   = extractHsTyNames ty
hsSigFVs other		    = emptyFVs

----------------
conDeclFVs (ConDecl _ tyvars context details _)
  = delFVs (map hsTyVarName tyvars) $
    extractHsCtxtTyNames context	  `plusFV`
    conDetailsFVs details

conDetailsFVs (PrefixCon btys)    = plusFVs (map bangTyFVs btys)
conDetailsFVs (InfixCon bty1 bty2) = bangTyFVs bty1 `plusFV` bangTyFVs bty2
conDetailsFVs (RecCon flds)	   = plusFVs [bangTyFVs bty | (_, bty) <- flds]

bangTyFVs bty = extractHsTyNames (getBangType bty)
\end{code}


%************************************************************************
%*									*
\subsection{A few functions on generic defintions
%*									*
%************************************************************************

These functions on generics are defined over RenamedMatches, which is
why they are here and not in HsMatches.

\begin{code}
maybeGenericMatch :: RenamedMatch -> Maybe (RenamedHsType, RenamedMatch)
  -- Tells whether a Match is for a generic definition
  -- and extract the type from a generic match and put it at the front

maybeGenericMatch (Match (TypePat ty : pats) sig_ty grhss)
  = Just (ty, Match pats sig_ty grhss)

maybeGenericMatch other_match = Nothing
\end{code}
