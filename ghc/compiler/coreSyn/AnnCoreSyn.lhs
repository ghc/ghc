%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[AnnCoreSyntax]{Annotated core syntax}

For when you want @CoreSyntax@ trees annotated at every node.  Other
than that, just like @CoreSyntax@.  (Important to be sure that it {\em
really is} just like @CoreSyntax@.)

\begin{code}
#include "HsVersions.h"

module AnnCoreSyn (
	AnnCoreBinding(..), AnnCoreExpr(..),
	AnnCoreExpr'(..),	-- v sad that this must be exported
	AnnCoreCaseAlts(..), AnnCoreCaseDefault(..),

	deAnnotate -- we may eventually export some of the other deAnners
    ) where

import Ubiq{-uitous-}

import CoreSyn
\end{code}

\begin{code}
data AnnCoreBinding val_bdr val_occ tyvar uvar annot
  = AnnNonRec val_bdr (AnnCoreExpr val_bdr val_occ tyvar uvar annot)
  | AnnRec    [(val_bdr, AnnCoreExpr val_bdr val_occ tyvar uvar annot)]
\end{code}

\begin{code}
type AnnCoreExpr val_bdr val_occ tyvar uvar annot
  = (annot, AnnCoreExpr' val_bdr val_occ tyvar uvar annot)

data AnnCoreExpr' val_bdr val_occ tyvar uvar annot
  = AnnVar	val_occ
  | AnnLit 	Literal

  | AnnCon	Id     [GenCoreArg val_occ tyvar uvar]
  | AnnPrim	PrimOp [GenCoreArg val_occ tyvar uvar]

  | AnnLam	(GenCoreBinder val_bdr tyvar uvar)
		(AnnCoreExpr val_bdr val_occ tyvar uvar annot)

  | AnnApp	(AnnCoreExpr val_bdr val_occ tyvar uvar annot)
		(GenCoreArg  val_occ tyvar uvar)

  | AnnCase	(AnnCoreExpr val_bdr val_occ tyvar uvar annot)
		(AnnCoreCaseAlts val_bdr val_occ tyvar uvar annot)

  | AnnLet	(AnnCoreBinding val_bdr val_occ tyvar uvar annot)
		(AnnCoreExpr val_bdr val_occ tyvar uvar annot)

  | AnnSCC	CostCentre
		(AnnCoreExpr val_bdr val_occ tyvar uvar annot)
\end{code}

\begin{code}
data AnnCoreCaseAlts val_bdr val_occ tyvar uvar annot
  = AnnAlgAlts	[(Id,
		  [val_bdr],
		  AnnCoreExpr val_bdr val_occ tyvar uvar annot)]
		(AnnCoreCaseDefault val_bdr val_occ tyvar uvar annot)
  | AnnPrimAlts	[(Literal,
		  AnnCoreExpr val_bdr val_occ tyvar uvar annot)]
		(AnnCoreCaseDefault val_bdr val_occ tyvar uvar annot)

data AnnCoreCaseDefault val_bdr val_occ tyvar uvar annot
  = AnnNoDefault
  | AnnBindDefault  val_bdr
		    (AnnCoreExpr val_bdr val_occ tyvar uvar annot)
\end{code}

\begin{code}
deAnnotate :: AnnCoreExpr val_bdr val_occ tyvar uvar ann
	   -> GenCoreExpr val_bdr val_occ tyvar uvar

deAnnotate (_, AnnVar	v)          = Var v
deAnnotate (_, AnnLit	lit)	    = Lit lit
deAnnotate (_, AnnCon	con args)   = Con con args
deAnnotate (_, AnnPrim	op args)    = Prim op args
deAnnotate (_, AnnLam	binder body)= Lam binder (deAnnotate body)
deAnnotate (_, AnnApp	fun arg)    = App (deAnnotate fun) arg
deAnnotate (_, AnnSCC	lbl body)   = SCC lbl (deAnnotate body)

deAnnotate (_, AnnLet bind body)
  = Let (deAnnBind bind) (deAnnotate body)
  where
    deAnnBind (AnnNonRec var rhs) = NonRec var (deAnnotate rhs)
    deAnnBind (AnnRec pairs) = Rec [(v,deAnnotate rhs) | (v,rhs) <- pairs]

deAnnotate (_, AnnCase scrut alts)
  = Case (deAnnotate scrut) (deAnnAlts alts)
  where
    deAnnAlts (AnnAlgAlts alts deflt)
      = AlgAlts [(con,args,deAnnotate rhs) | (con,args,rhs) <- alts]
		 (deAnnDeflt deflt)

    deAnnAlts (AnnPrimAlts alts deflt)
      = PrimAlts [(lit,deAnnotate rhs) | (lit,rhs) <- alts]
		   (deAnnDeflt deflt)

    deAnnDeflt AnnNoDefault 	        = NoDefault
    deAnnDeflt (AnnBindDefault var rhs) = BindDefault var (deAnnotate rhs)
\end{code}
