%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
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

	-- and to make the interface self-sufficient
    ) where

import PrelInfo		( PrimOp(..), PrimRep
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import Literal		( Literal )
import CoreSyn
import Outputable
import CostCentre	( CostCentre )
#if USE_ATTACK_PRAGMAS
import Util
#endif
\end{code}

\begin{code}
data AnnCoreBinding binder bindee annot
  = AnnCoNonRec binder (AnnCoreExpr binder bindee annot)
  | AnnCoRec	[(binder, AnnCoreExpr binder bindee annot)]
\end{code}

\begin{code}
type AnnCoreExpr binder bindee annot = (annot, AnnCoreExpr' binder bindee annot)

data AnnCoreExpr' binder bindee annot
  = AnnCoVar	 bindee
  | AnnCoLit Literal

  | AnnCoCon	 Id [Type] [GenCoreAtom bindee]

  | AnnCoPrim    PrimOp [Type] [GenCoreAtom bindee]

  | AnnCoLam	 binder
		 (AnnCoreExpr binder bindee annot)
  | AnnCoTyLam   TyVar
		 (AnnCoreExpr binder bindee annot)

  | AnnCoApp	 (AnnCoreExpr binder bindee annot)
		 (GenCoreAtom    bindee)
  | AnnCoTyApp   (AnnCoreExpr binder bindee annot)
		 Type

  | AnnCoCase    (AnnCoreExpr binder bindee annot)
		 (AnnCoreCaseAlts binder bindee annot)

  | AnnCoLet	 (AnnCoreBinding binder bindee annot)
		 (AnnCoreExpr binder bindee annot)

  | AnnCoSCC	 CostCentre
		 (AnnCoreExpr binder bindee annot)
\end{code}

\begin{code}
data AnnCoreCaseAlts binder bindee annot
  = AnnCoAlgAlts	[(Id,
		  	 [binder],
		  	 AnnCoreExpr binder bindee annot)]
			(AnnCoreCaseDefault binder bindee annot)
  | AnnCoPrimAlts	[(Literal,
			  AnnCoreExpr binder bindee annot)]
			(AnnCoreCaseDefault binder bindee annot)

data AnnCoreCaseDefault binder bindee annot
  = AnnCoNoDefault
  | AnnCoBindDefault	binder
			(AnnCoreExpr binder bindee annot)
\end{code}

\begin{code}
deAnnotate :: AnnCoreExpr bndr bdee ann -> GenCoreExpr bndr bdee

deAnnotate (_, AnnCoVar v)            = Var v
deAnnotate (_, AnnCoLit lit)      = Lit lit
deAnnotate (_, AnnCoCon	con tys args) = Con con tys args
deAnnotate (_, AnnCoPrim op tys args) = Prim op tys args
deAnnotate (_, AnnCoLam	binder body)  = Lam binder (deAnnotate body)
deAnnotate (_, AnnCoTyLam tyvar body) = CoTyLam tyvar (deAnnotate body)
deAnnotate (_, AnnCoApp	fun arg)      = App (deAnnotate fun) arg
deAnnotate (_, AnnCoTyApp fun ty)     = CoTyApp (deAnnotate fun) ty
deAnnotate (_, AnnCoSCC	lbl body)     = SCC lbl (deAnnotate body)

deAnnotate (_, AnnCoLet	bind body)
  = Let (deAnnBind bind) (deAnnotate body)
  where
    deAnnBind (AnnCoNonRec var rhs) = NonRec var (deAnnotate rhs)
    deAnnBind (AnnCoRec pairs) = Rec [(v,deAnnotate rhs) | (v,rhs) <- pairs]

deAnnotate (_, AnnCoCase scrut alts)
  = Case (deAnnotate scrut) (deAnnAlts alts)
  where
    deAnnAlts (AnnCoAlgAlts alts deflt)
      = AlgAlts [(con,args,deAnnotate rhs) | (con,args,rhs) <- alts]
		 (deAnnDeflt deflt)

    deAnnAlts (AnnCoPrimAlts alts deflt)
      = PrimAlts [(lit,deAnnotate rhs) | (lit,rhs) <- alts]
		   (deAnnDeflt deflt)

    deAnnDeflt AnnCoNoDefault 	      = NoDefault
    deAnnDeflt (AnnCoBindDefault var rhs) = BindDefault var (deAnnotate rhs)
\end{code}
