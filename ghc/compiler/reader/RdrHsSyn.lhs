%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[RdrHsSyn]{Specialisations of the @HsSyn@ syntax for the reader}

(Well, really, for specialisations involving @RdrName@s, even if
they are used somewhat later on in the compiler...)

\begin{code}
#include "HsVersions.h"

module RdrHsSyn (
	RdrNameArithSeqInfo(..),
	RdrNameBangType(..),
	RdrNameBind(..),
	RdrNameClassDecl(..),
	RdrNameClassOpSig(..),
	RdrNameConDecl(..),
	RdrNameContext(..),
	RdrNameSpecDataSig(..),
	RdrNameDefaultDecl(..),
	RdrNameFixityDecl(..),
	RdrNameGRHS(..),
	RdrNameGRHSsAndBinds(..),
	RdrNameHsBinds(..),
	RdrNameHsExpr(..),
	RdrNameHsModule(..),
	RdrNameIE(..),
	RdrNameImportDecl(..),
	RdrNameInstDecl(..),
	RdrNameMatch(..),
	RdrNameMonoBinds(..),
	RdrNameMonoType(..),
	RdrNamePat(..),
	RdrNamePolyType(..),
	RdrNameQual(..),
	RdrNameSig(..),
	RdrNameSpecInstSig(..),
	RdrNameStmt(..),
	RdrNameTyDecl(..),

	RdrNameClassOpPragmas(..),
	RdrNameClassPragmas(..),
	RdrNameDataPragmas(..),
	RdrNameGenPragmas(..),
	RdrNameInstancePragmas(..),
	RdrNameCoreExpr(..),

	getRawImportees,
	getRawExportees
    ) where

import Ubiq

import HsSyn
import Name		( ExportFlag(..) )
\end{code}

\begin{code}
type RdrNameArithSeqInfo	= ArithSeqInfo		Fake Fake RdrName RdrNamePat
type RdrNameBangType		= BangType		RdrName
type RdrNameBind		= Bind			Fake Fake RdrName RdrNamePat
type RdrNameClassDecl		= ClassDecl		Fake Fake RdrName RdrNamePat
type RdrNameClassOpSig		= Sig			RdrName
type RdrNameConDecl		= ConDecl		RdrName
type RdrNameContext		= Context 		RdrName
type RdrNameSpecDataSig		= SpecDataSig		RdrName
type RdrNameDefaultDecl		= DefaultDecl		RdrName
type RdrNameFixityDecl		= FixityDecl		RdrName
type RdrNameGRHS		= GRHS			Fake Fake RdrName RdrNamePat
type RdrNameGRHSsAndBinds	= GRHSsAndBinds		Fake Fake RdrName RdrNamePat
type RdrNameHsBinds		= HsBinds		Fake Fake RdrName RdrNamePat
type RdrNameHsExpr		= HsExpr		Fake Fake RdrName RdrNamePat
type RdrNameHsModule		= HsModule		Fake Fake RdrName RdrNamePat
type RdrNameIE			= IE			RdrName
type RdrNameImportDecl 		= ImportDecl		RdrName
type RdrNameInstDecl		= InstDecl		Fake Fake RdrName RdrNamePat
type RdrNameMatch		= Match			Fake Fake RdrName RdrNamePat
type RdrNameMonoBinds		= MonoBinds		Fake Fake RdrName RdrNamePat
type RdrNameMonoType		= MonoType		RdrName
type RdrNamePat			= InPat			RdrName
type RdrNamePolyType		= PolyType		RdrName
type RdrNameQual		= Qual			Fake Fake RdrName RdrNamePat
type RdrNameSig			= Sig			RdrName
type RdrNameSpecInstSig		= SpecInstSig 		RdrName
type RdrNameStmt		= Stmt			Fake Fake RdrName RdrNamePat
type RdrNameTyDecl		= TyDecl		RdrName

type RdrNameClassOpPragmas	= ClassOpPragmas	RdrName
type RdrNameClassPragmas	= ClassPragmas		RdrName
type RdrNameDataPragmas		= DataPragmas		RdrName
type RdrNameGenPragmas		= GenPragmas		RdrName
type RdrNameInstancePragmas	= InstancePragmas	RdrName
type RdrNameCoreExpr		= UnfoldingCoreExpr	RdrName
\end{code}

%************************************************************************
%*									*
\subsection{Grabbing importees and exportees}
%*									*
%************************************************************************

\begin{code}
getRawImportees :: [RdrNameIE] ->  [RdrName]
getRawExportees :: Maybe [RdrNameIE] -> ([(RdrName, ExportFlag)], [Module])

getRawImportees imps
  = foldr do_imp [] imps
  where
    do_imp (IEVar n)	     acc = n:acc
    do_imp (IEThingAbs  n)   acc = n:acc
    do_imp (IEThingWith n _) acc = n:acc
    do_imp (IEThingAll  n)   acc = n:acc

getRawExportees Nothing     = ([], [])
getRawExportees (Just exps)
  = foldr do_exp ([],[]) exps
  where
    do_exp (IEVar n)		(prs, mods) = ((n, ExportAll):prs, mods)
    do_exp (IEThingAbs n)	(prs, mods) = ((n, ExportAbs):prs, mods)
    do_exp (IEThingAll n)	(prs, mods) = ((n, ExportAll):prs, mods)
    do_exp (IEThingWith n _)	(prs, mods) = ((n, ExportAll):prs, mods)
    do_exp (IEModuleContents n) (prs, mods) = (prs, n : mods)
\end{code}
