%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RnHsSyn]{Specialisations of the @HsSyn@ syntax for the renamer}

\begin{code}
module RnHsSyn where

#include "HsVersions.h"

import HsSyn
import HsPragmas	( InstancePragmas, GenPragmas, DataPragmas, ClassPragmas, ClassOpPragmas )

import TysWiredIn	( tupleTyCon, unboxedTupleTyCon, 
			  listTyCon, charTyCon )
import Name		( Name, getName )
import NameSet
import Util
import Outputable
\end{code}


\begin{code}
type RenamedArithSeqInfo	= ArithSeqInfo		Name RenamedPat
type RenamedClassOpSig		= Sig			Name
type RenamedConDecl		= ConDecl		Name
type RenamedContext		= Context 		Name
type RenamedHsDecl		= HsDecl		Name RenamedPat
type RenamedRuleDecl		= RuleDecl		Name RenamedPat
type RenamedTyClDecl		= TyClDecl		Name RenamedPat
type RenamedSpecDataSig		= SpecDataSig		Name
type RenamedDefaultDecl		= DefaultDecl		Name
type RenamedForeignDecl		= ForeignDecl		Name
type RenamedGRHS		= GRHS			Name RenamedPat
type RenamedGRHSs		= GRHSs			Name RenamedPat
type RenamedHsBinds		= HsBinds		Name RenamedPat
type RenamedHsExpr		= HsExpr		Name RenamedPat
type RenamedHsModule		= HsModule		Name RenamedPat
type RenamedInstDecl		= InstDecl		Name RenamedPat
type RenamedMatch		= Match			Name RenamedPat
type RenamedMonoBinds		= MonoBinds		Name RenamedPat
type RenamedPat			= InPat			Name
type RenamedHsType		= HsType		Name
type RenamedRecordBinds		= HsRecordBinds		Name RenamedPat
type RenamedSig			= Sig			Name
type RenamedStmt		= Stmt			Name RenamedPat
type RenamedFixitySig		= FixitySig		Name

type RenamedClassOpPragmas	= ClassOpPragmas	Name
type RenamedClassPragmas	= ClassPragmas		Name
type RenamedDataPragmas		= DataPragmas		Name
type RenamedGenPragmas		= GenPragmas		Name
type RenamedInstancePragmas	= InstancePragmas	Name
\end{code}

%************************************************************************
%*									*
\subsection{Free variables}
%*									*
%************************************************************************

These free-variable finders returns tycons and classes too.

\begin{code}
charTyCon_name, listTyCon_name :: Name
charTyCon_name    = getName charTyCon
listTyCon_name    = getName listTyCon

tupleTyCon_name :: Bool -> Int -> Name
tupleTyCon_name True  n = getName (tupleTyCon n)
tupleTyCon_name False n = getName (unboxedTupleTyCon n)

extractHsTyNames   :: RenamedHsType -> NameSet
extractHsTyNames ty
  = get ty
  where
    get (MonoTyApp ty1 ty2)      = get ty1 `unionNameSets` get ty2
    get (MonoListTy ty)          = unitNameSet listTyCon_name 
				   `unionNameSets` get ty
    get (MonoTupleTy tys boxed)  = unitNameSet (tupleTyCon_name boxed (length tys)) 
				   `unionNameSets` extractHsTyNames_s tys
    get (MonoFunTy ty1 ty2)      = get ty1 `unionNameSets` get ty2
    get (MonoDictTy cls tys)     = unitNameSet cls `unionNameSets` extractHsTyNames_s tys
    get (MonoTyVar tv)	         = unitNameSet tv
    get (HsForAllTy (Just tvs) 
		    ctxt ty)     = (extractHsCtxtTyNames ctxt `unionNameSets` get ty)
					    `minusNameSet`
				    mkNameSet (map getTyVarName tvs)
    get ty@(HsForAllTy Nothing _ _) = pprPanic "extractHsTyNames" (ppr ty)

extractHsTyNames_s  :: [RenamedHsType] -> NameSet
extractHsTyNames_s tys = foldr (unionNameSets . extractHsTyNames) emptyNameSet tys

extractHsCtxtTyNames :: RenamedContext -> NameSet
extractHsCtxtTyNames ctxt = foldr (unionNameSets . get) emptyNameSet ctxt
  where
    get (cls, tys) = unitNameSet cls `unionNameSets` extractHsTyNames_s tys
\end{code}

