%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[RnHsSyn]{Specialisations of the @HsSyn@ syntax for the renamer}

\begin{code}
#include "HsVersions.h"

module RnHsSyn where

IMP_Ubiq()

import HsSyn
#if __GLASGOW_HASKELL__ >= 202
import HsPragmas
#endif

import Id		( GenId, SYN_IE(Id) )
import BasicTypes	( NewOrData )
import Name		( Name )
import Outputable	( PprStyle(..), Outputable(..){-instance * []-} )
import PprType		( GenType, GenTyVar, TyCon )
import Pretty
import Name		( SYN_IE(NameSet), unitNameSet, mkNameSet, minusNameSet, unionNameSets, emptyNameSet )
import TyCon		( TyCon )
import TyVar		( GenTyVar )
import Unique		( Unique )
import Util		( panic, pprPanic{-, pprTrace ToDo:rm-} )
\end{code}


\begin{code}
type RenamedArithSeqInfo	= ArithSeqInfo		Fake Fake Name RenamedPat
type RenamedClassDecl		= ClassDecl		Fake Fake Name RenamedPat
type RenamedClassOpSig		= Sig			Name
type RenamedConDecl		= ConDecl		Name
type RenamedContext		= Context 		Name
type RenamedHsDecl		= HsDecl		Fake Fake Name RenamedPat
type RenamedSpecDataSig		= SpecDataSig		Name
type RenamedDefaultDecl		= DefaultDecl		Name
type RenamedFixityDecl		= FixityDecl		Name
type RenamedGRHS		= GRHS			Fake Fake Name RenamedPat
type RenamedGRHSsAndBinds	= GRHSsAndBinds		Fake Fake Name RenamedPat
type RenamedHsBinds		= HsBinds		Fake Fake Name RenamedPat
type RenamedHsExpr		= HsExpr		Fake Fake Name RenamedPat
type RenamedHsModule		= HsModule		Fake Fake Name RenamedPat
type RenamedInstDecl		= InstDecl		Fake Fake Name RenamedPat
type RenamedMatch		= Match			Fake Fake Name RenamedPat
type RenamedMonoBinds		= MonoBinds		Fake Fake Name RenamedPat
type RenamedPat			= InPat			Name
type RenamedHsType		= HsType		Name
type RenamedRecordBinds		= HsRecordBinds		Fake Fake Name RenamedPat
type RenamedSig			= Sig			Name
type RenamedSpecInstSig		= SpecInstSig 		Name
type RenamedStmt		= Stmt			Fake Fake Name RenamedPat
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

\begin{code}
extractCtxtTyNames :: RenamedContext -> NameSet
extractCtxtTyNames ctxt = foldr (unionNameSets . extractHsTyNames . snd) emptyNameSet ctxt

extractHsTyNames   :: RenamedHsType  -> NameSet
extractHsTyNames ty
  = get ty
  where
    get (MonoTyApp ty1 ty2)      = get ty1 `unionNameSets` get ty2
    get (MonoListTy tc ty)       = unitNameSet tc `unionNameSets` get ty
    get (MonoTupleTy tc tys)     = foldr (unionNameSets . get) (unitNameSet tc) tys
    get (MonoFunTy ty1 ty2)      = get ty1 `unionNameSets` get ty2
    get (MonoDictTy cls ty)      = unitNameSet cls `unionNameSets` get ty
    get (MonoTyVar tv)	         = unitNameSet tv
    get (HsForAllTy tvs ctxt ty) = foldr (unionNameSets . get . snd) (get ty) ctxt
					    `minusNameSet`
				    mkNameSet (map getTyVarName tvs)

\end{code}

