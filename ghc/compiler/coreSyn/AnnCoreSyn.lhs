%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[AnnCoreSyntax]{Annotated core syntax}

For when you want @CoreSyntax@ trees annotated at every node.  Other
than that, just like @CoreSyntax@.  (Important to be sure that it {\em
really is} just like @CoreSyntax@.)

\begin{code}
module AnnCoreSyn (
	AnnCoreBinding(..), AnnCoreExpr,
	AnnCoreExpr'(..),	-- v sad that this must be exported
	AnnCoreCaseAlts(..), AnnCoreCaseDefault(..),

	deAnnotate -- we may eventually export some of the other deAnners
    ) where

#include "HsVersions.h"

import CoreSyn

import Id         ( Id )
import Literal    ( Literal )
import PrimOp     ( PrimOp )
import CostCentre ( CostCentre )
import Type       ( GenType )

\end{code}

\begin{code}
data AnnCoreBinding val_bdr val_occ flexi annot
  = AnnNonRec val_bdr (AnnCoreExpr val_bdr val_occ flexi annot)
  | AnnRec    [(val_bdr, AnnCoreExpr val_bdr val_occ flexi annot)]
\end{code}

\begin{code}
type AnnCoreExpr val_bdr val_occ flexi annot
  = (annot, AnnCoreExpr' val_bdr val_occ flexi annot)

data AnnCoreExpr' val_bdr val_occ flexi annot
  = AnnVar	val_occ
  | AnnLit 	Literal

  | AnnCon	Id     [GenCoreArg val_occ flexi]
  | AnnPrim	PrimOp [GenCoreArg val_occ flexi]

  | AnnLam	(GenCoreBinder val_bdr flexi)
		(AnnCoreExpr val_bdr val_occ flexi annot)

  | AnnApp	(AnnCoreExpr val_bdr val_occ flexi annot)
		(GenCoreArg  val_occ flexi)

  | AnnCase	(AnnCoreExpr val_bdr val_occ flexi annot)
		(AnnCoreCaseAlts val_bdr val_occ flexi annot)

  | AnnLet	(AnnCoreBinding val_bdr val_occ flexi annot)
		(AnnCoreExpr val_bdr val_occ flexi annot)

  | AnnNote	(CoreNote flexi)
		(AnnCoreExpr val_bdr val_occ flexi annot)
\end{code}

\begin{code}
data AnnCoreCaseAlts val_bdr val_occ flexi annot
  = AnnAlgAlts	[(Id,
		  [val_bdr],
		  AnnCoreExpr val_bdr val_occ flexi annot)]
		(AnnCoreCaseDefault val_bdr val_occ flexi annot)
  | AnnPrimAlts	[(Literal,
		  AnnCoreExpr val_bdr val_occ flexi annot)]
		(AnnCoreCaseDefault val_bdr val_occ flexi annot)

data AnnCoreCaseDefault val_bdr val_occ flexi annot
  = AnnNoDefault
  | AnnBindDefault  val_bdr
		    (AnnCoreExpr val_bdr val_occ flexi annot)
\end{code}

\begin{code}
deAnnotate :: AnnCoreExpr val_bdr val_occ flexi ann
	   -> GenCoreExpr val_bdr val_occ flexi

deAnnotate (_, AnnVar	v)          = Var v
deAnnotate (_, AnnLit	lit)	    = Lit lit
deAnnotate (_, AnnCon	con args)   = Con con args
deAnnotate (_, AnnPrim	op args)    = Prim op args
deAnnotate (_, AnnLam	binder body)= Lam binder (deAnnotate body)
deAnnotate (_, AnnApp	fun arg)    = App (deAnnotate fun) arg
deAnnotate (_, AnnNote	note body)  = Note note (deAnnotate body)

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
