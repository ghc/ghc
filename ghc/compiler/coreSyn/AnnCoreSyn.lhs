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
	AnnCoreCaseAlternatives(..), AnnCoreCaseDefault(..),
#ifdef DPH
	AnnCoreParQuals(..),
        AnnCoreParCommunicate(..),
#endif {- Data Parallel Haskell -}

	deAnnotate, -- we may eventually export some of the other deAnners

	-- and to make the interface self-sufficient
	BasicLit, Id, PrimOp, TyCon, TyVar, UniType, CostCentre
	IF_ATTACK_PRAGMAS(COMMA cmpTyCon COMMA cmpTyVar)
	IF_ATTACK_PRAGMAS(COMMA cmpUniType)
    ) where

import AbsPrel		( PrimOp(..), PrimKind
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType	( Id, TyVar, TyCon, UniType
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon COMMA cmpTyVar)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import BasicLit		( BasicLit )
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
  | AnnCoLit BasicLit

  | AnnCoCon	 Id [UniType] [CoreAtom bindee]

  | AnnCoPrim    PrimOp [UniType] [CoreAtom bindee]

  | AnnCoLam	 [binder]
		 (AnnCoreExpr binder bindee annot)
  | AnnCoTyLam   TyVar
		 (AnnCoreExpr binder bindee annot)

  | AnnCoApp	 (AnnCoreExpr binder bindee annot)
		 (CoreAtom    bindee)
  | AnnCoTyApp   (AnnCoreExpr binder bindee annot)
		 UniType

  | AnnCoCase    (AnnCoreExpr binder bindee annot)
		 (AnnCoreCaseAlternatives binder bindee annot)

  | AnnCoLet	 (AnnCoreBinding binder bindee annot)
		 (AnnCoreExpr binder bindee annot)

  | AnnCoSCC	 CostCentre
		 (AnnCoreExpr binder bindee annot)
#ifdef DPH
  | AnnCoZfExpr  (AnnCoreExpr binder bindee annot) 
	         (AnnCoreParQuals binder bindee annot)

  | AnnCoParCon	 Id Int [UniType] [AnnCoreExpr binder bindee annot]

  | AnnCoParComm
 		     Int
		    (AnnCoreExpr binder bindee annot)
		    (AnnCoreParCommunicate binder bindee annot)
  | AnnCoParZipWith
		     Int 
		     (AnnCoreExpr binder bindee annot)
		     [AnnCoreExpr binder bindee annot]
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
#ifdef DPH
data AnnCoreParQuals binder bindee annot
   = AnnCoAndQuals  (AnnCoreParQuals binder bindee annot)
		    (AnnCoreParQuals binder bindee annot)
   | AnnCoParFilter (AnnCoreExpr binder bindee annot)
   | AnnCoDrawnGen  [binder]
		    (binder)
		    (AnnCoreExpr binder bindee annot)	
   | AnnCoIndexGen  [AnnCoreExpr binder bindee annot]
   		    (binder)
		    (AnnCoreExpr binder bindee annot)	
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
data AnnCoreCaseAlternatives binder bindee annot
  = AnnCoAlgAlts	[(Id,
		  	 [binder],
		  	 AnnCoreExpr binder bindee annot)]
			(AnnCoreCaseDefault binder bindee annot)
  | AnnCoPrimAlts	[(BasicLit,
			  AnnCoreExpr binder bindee annot)]
			(AnnCoreCaseDefault binder bindee annot)
#ifdef DPH
  | AnnCoParAlgAlts	TyCon	
		        Int
			[binder]
			[(Id,
		  	 AnnCoreExpr binder bindee annot)]
			(AnnCoreCaseDefault binder bindee annot)
  | AnnCoParPrimAlts	TyCon	
			Int
			[(BasicLit,
			  AnnCoreExpr binder bindee annot)]
			(AnnCoreCaseDefault binder bindee annot)
#endif {- Data Parallel Haskell -}

data AnnCoreCaseDefault binder bindee annot
  = AnnCoNoDefault
  | AnnCoBindDefault	binder
			(AnnCoreExpr binder bindee annot)
\end{code}

\begin{code}
#ifdef DPH
data AnnCoreParCommunicate binder bindee annot
  = AnnCoParSend	[AnnCoreExpr binder bindee annot]     
  | AnnCoParFetch  	[AnnCoreExpr binder bindee annot]     
  | AnnCoToPodized
  | AnnCoFromPodized
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
deAnnotate :: AnnCoreExpr bndr bdee ann -> CoreExpr bndr bdee

deAnnotate (_, AnnCoVar v)            = CoVar v
deAnnotate (_, AnnCoLit lit)      = CoLit lit
deAnnotate (_, AnnCoCon	con tys args) = CoCon con tys args
deAnnotate (_, AnnCoPrim op tys args) = CoPrim op tys args
deAnnotate (_, AnnCoLam	binders body) = CoLam binders (deAnnotate body)
deAnnotate (_, AnnCoTyLam tyvar body) = CoTyLam tyvar (deAnnotate body)
deAnnotate (_, AnnCoApp	fun arg)      = CoApp (deAnnotate fun) arg
deAnnotate (_, AnnCoTyApp fun ty)     = CoTyApp (deAnnotate fun) ty
deAnnotate (_, AnnCoSCC	lbl body)     = CoSCC lbl (deAnnotate body) 

deAnnotate (_, AnnCoLet	bind body)
  = CoLet (deAnnBind bind) (deAnnotate body)
  where
    deAnnBind (AnnCoNonRec var rhs) = CoNonRec var (deAnnotate rhs)
    deAnnBind (AnnCoRec pairs) = CoRec [(v,deAnnotate rhs) | (v,rhs) <- pairs]

deAnnotate (_, AnnCoCase scrut alts)
  = CoCase (deAnnotate scrut) (deAnnAlts alts)
  where
    deAnnAlts (AnnCoAlgAlts alts deflt)  
      = CoAlgAlts [(con,args,deAnnotate rhs) | (con,args,rhs) <- alts]
		 (deAnnDeflt deflt)

    deAnnAlts (AnnCoPrimAlts alts deflt) 
      = CoPrimAlts [(lit,deAnnotate rhs) | (lit,rhs) <- alts]
		   (deAnnDeflt deflt)

    deAnnDeflt AnnCoNoDefault 	      = CoNoDefault
    deAnnDeflt (AnnCoBindDefault var rhs) = CoBindDefault var (deAnnotate rhs)
\end{code}
