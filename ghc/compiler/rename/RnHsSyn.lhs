%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RnHsSyn]{Specialisations of the @HsSyn@ syntax for the renamer}

\begin{code}
module RnHsSyn where

#include "HsVersions.h"

import RnEnv		( listTyCon_name, tupleTyCon_name )

import HsSyn
import HsPragmas	( InstancePragmas, GenPragmas, DataPragmas, ClassPragmas, ClassOpPragmas )

import BasicTypes	( Unused )
import Name		( Name )
import NameSet
import Util
import Outputable
\end{code}


\begin{code}
type RenamedArithSeqInfo	= ArithSeqInfo		Unused Name RenamedPat
type RenamedClassDecl		= ClassDecl		Unused Name RenamedPat
type RenamedClassOpSig		= Sig			Name
type RenamedConDecl		= ConDecl		Name
type RenamedContext		= Context 		Name
type RenamedHsDecl		= HsDecl		Unused Name RenamedPat
type RenamedSpecDataSig		= SpecDataSig		Name
type RenamedDefaultDecl		= DefaultDecl		Name
type RenamedForeignDecl		= ForeignDecl		Name
type RenamedFixityDecl		= FixityDecl		Name
type RenamedGRHS		= GRHS			Unused Name RenamedPat
type RenamedGRHSsAndBinds	= GRHSsAndBinds		Unused Name RenamedPat
type RenamedHsBinds		= HsBinds		Unused Name RenamedPat
type RenamedHsExpr		= HsExpr		Unused Name RenamedPat
type RenamedHsModule		= HsModule		Unused Name RenamedPat
type RenamedInstDecl		= InstDecl		Unused Name RenamedPat
type RenamedMatch		= Match			Unused Name RenamedPat
type RenamedMonoBinds		= MonoBinds		Unused Name RenamedPat
type RenamedPat			= InPat			Name
type RenamedHsType		= HsType		Name
type RenamedRecordBinds		= HsRecordBinds		Unused Name RenamedPat
type RenamedSig			= Sig			Name
type RenamedStmt		= Stmt			Unused Name RenamedPat
type RenamedTyDecl		= TyDecl		Name

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
    get (HsForAllTy tvs ctxt ty) = (extractHsCtxtTyNames ctxt `unionNameSets` get ty)
					    `minusNameSet`
				    mkNameSet (map getTyVarName tvs)

extractHsTyNames_s  :: [RenamedHsType] -> NameSet
extractHsTyNames_s tys = foldr (unionNameSets . extractHsTyNames) emptyNameSet tys

extractHsCtxtTyNames :: RenamedContext -> NameSet
extractHsCtxtTyNames ctxt = foldr (unionNameSets . get) emptyNameSet ctxt
  where
    get (cls, tys) = unitNameSet cls `unionNameSets` extractHsTyNames_s tys
\end{code}

