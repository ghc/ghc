%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[RnHsSyn]{Specialisations of the @HsSyn@ syntax for the renamer}

\begin{code}
#include "HsVersions.h"

module RnHsSyn where

import Ubiq{-uitous-}

import HsSyn
\end{code}

\begin{code}
type RenamedArithSeqInfo	= ArithSeqInfo		Fake Fake Name RenamedPat
type RenamedBind		= Bind			Fake Fake Name RenamedPat
type RenamedClassDecl		= ClassDecl		Fake Fake Name RenamedPat
type RenamedClassOpPragmas	= ClassOpPragmas	Name
type RenamedClassOpSig		= Sig			Name
type RenamedClassPragmas	= ClassPragmas		Name
type RenamedConDecl		= ConDecl		Name
type RenamedContext		= Context 		Name
type RenamedDataPragmas		= DataPragmas		Name
type RenamedSpecDataSig		= SpecDataSig		Name
type RenamedDefaultDecl		= DefaultDecl		Name
type RenamedFixityDecl		= FixityDecl		Name
type RenamedGRHS		= GRHS			Fake Fake Name RenamedPat
type RenamedGRHSsAndBinds	= GRHSsAndBinds		Fake Fake Name RenamedPat
type RenamedGenPragmas		= GenPragmas		Name
type RenamedHsBinds		= HsBinds		Fake Fake Name RenamedPat
type RenamedHsExpr		= HsExpr		Fake Fake Name RenamedPat
type RenamedHsModule		= HsModule		Fake Fake Name RenamedPat
type RenamedImportedInterface	= ImportedInterface	Fake Fake Name RenamedPat
type RenamedInstDecl		= InstDecl		Fake Fake Name RenamedPat
type RenamedInstancePragmas	= InstancePragmas	Name
type RenamedInterface		= Interface		Fake Fake Name RenamedPat
type RenamedMatch		= Match			Fake Fake Name RenamedPat
type RenamedMonoBinds		= MonoBinds		Fake Fake Name RenamedPat
type RenamedMonoType		= MonoType		Name
type RenamedPat			= InPat			Name
type RenamedPolyType		= PolyType		Name
type RenamedQual		= Qual			Fake Fake Name RenamedPat
type RenamedSig			= Sig			Name
type RenamedSpecInstSig		= SpecInstSig 		Name
type RenamedStmt		= Stmt			Fake Fake Name RenamedPat
type RenamedTyDecl		= TyDecl		Name
\end{code}

\begin{code}
collectQualBinders :: [RenamedQual] -> [Name]

collectQualBinders quals
  = concat (map collect quals)
  where
    collect (GeneratorQual pat _) = collectPatBinders pat
    collect (FilterQual expr)	  = []
    collect (LetQual    binds)	  = collectTopLevelBinders binds
\end{code}
