%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
%************************************************************************
%*									*
\section[HsCore]{Core-syntax unfoldings in Haskell interface files}
%*									*
%************************************************************************

We could either use this, or parameterise @GenCoreExpr@ on @Types@ and
@TyVars@ as well.  Currently trying the former... MEGA SIGH.

\begin{code}
module HsCore (
	UfExpr(..), UfAlt, UfBinder(..), UfNote(..),
	UfBinding(..), UfConAlt(..),
	HsIdInfo(..), HsStrictnessInfo(..),
	IfaceSig(..), UfRuleBody(..)
    ) where

#include "HsVersions.h"

-- friends:
import HsTypes		( HsType, pprParendHsType )

-- others:
import IdInfo		( ArityInfo, UpdateInfo, InlinePragInfo )
import CoreSyn		( CoreBndr, CoreExpr )
import Demand		( Demand )
import Literal		( Literal )
import PrimOp		( CCall, pprCCallOp )
import Type		( Kind )
import CostCentre
import SrcLoc		( SrcLoc )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[HsCore-types]{Types for read/written Core unfoldings}
%*									*
%************************************************************************

\begin{code}
data UfExpr name
  = UfVar 	name
  | UfType      (HsType name)
  | UfTuple 	name [UfExpr name]		-- Type arguments omitted
  | UfLam 	(UfBinder name)	  (UfExpr name)
  | UfApp 	(UfExpr name) (UfExpr name)
  | UfCase	(UfExpr name) name [UfAlt name]
  | UfLet	(UfBinding name)  (UfExpr name)
  | UfNote	(UfNote name) (UfExpr name)
  | UfLit	Literal
  | UfLitLit	FAST_STRING (HsType name)
  | UfCCall	CCall (HsType name)

data UfNote name = UfSCC CostCentre
	         | UfCoerce (HsType name)
	         | UfInlineCall
	         | UfInlineMe

type UfAlt name = (UfConAlt name, [name], UfExpr name)

data UfConAlt name = UfDefault
 		   | UfDataAlt name
		   | UfLitAlt Literal
		   | UfLitLitAlt FAST_STRING (HsType name)

data UfBinding name
  = UfNonRec	(UfBinder name)
		(UfExpr name)
  | UfRec 	[(UfBinder name, UfExpr name)]

data UfBinder name
  = UfValBinder	name (HsType name)
  | UfTyBinder	name Kind
\end{code}


%************************************************************************
%*									*
\subsection[HsCore-print]{Printing Core unfoldings}
%*									*
%************************************************************************

\begin{code}
instance Outputable name => Outputable (UfExpr name) where
    ppr (UfVar v) = ppr v
    ppr (UfLit l) = ppr l

    ppr (UfLitLit l ty) = ppr l
    ppr (UfCCall cc ty) = pprCCallOp cc

    ppr (UfType ty) = char '@' <+> pprParendHsType ty

    ppr (UfTuple c as) = parens (hsep (punctuate comma (map ppr as)))

    ppr (UfLam b body)
      = hsep [char '\\', ppr b, ptext SLIT("->"), ppr body]

    ppr (UfApp fun arg) = ppr fun <+> ppr arg 

    ppr (UfCase scrut bndr alts)
      = hsep [ptext SLIT("case"), ppr scrut, ptext SLIT("of"), ppr bndr,
	      braces (hsep (punctuate semi (map pp_alt alts)))]
      where
	pp_alt (c,bs,rhs) = hsep [ppr c, ppr bs, ppr_arrow, ppr rhs]

        ppr_arrow = ptext SLIT("->")

    ppr (UfLet (UfNonRec b rhs) body)
      = hsep [ptext SLIT("let"), ppr b, equals, ppr rhs, ptext SLIT("in"), ppr body]
    ppr (UfLet (UfRec pairs) body)
      = hsep [ptext SLIT("letrec"), braces (hsep (punctuate semi (map pp_pair pairs))), ptext SLIT("in"), ppr body]
      where
	pp_pair (b,rhs) = hsep [ppr b, equals, ppr rhs]

    ppr (UfNote note body)
      = hsep [ptext SLIT("_NOTE_ [ToDo]>"), ppr body]

instance Outputable name => Outputable (UfConAlt name) where
    ppr UfDefault	   = text "DEFAULT"
    ppr (UfLitAlt l)       = ppr l
    ppr (UfLitLitAlt l ty) = ppr l
    ppr (UfDataAlt d)	   = ppr d

instance Outputable name => Outputable (UfBinder name) where
    ppr (UfValBinder name ty)  = hsep [ppr name, dcolon, ppr ty]
    ppr (UfTyBinder name kind) = hsep [ppr name, dcolon, ppr kind]
\end{code}


%************************************************************************
%*									*
\subsection{Signatures in interface files}
%*									*
%************************************************************************

\begin{code}
data IfaceSig name
  = IfaceSig	name
		(HsType name)
		[HsIdInfo name]
		SrcLoc

instance (Outputable name) => Outputable (IfaceSig name) where
    ppr (IfaceSig var ty info _)
      = hang (hsep [ppr var, dcolon])
	     4 (ppr ty $$ ifPprDebug (vcat (map ppr info)))

data HsIdInfo name
  = HsArity		ArityInfo
  | HsStrictness	HsStrictnessInfo
  | HsUnfold		InlinePragInfo (UfExpr name)
  | HsUpdate		UpdateInfo
  | HsSpecialise	(UfRuleBody name)
  | HsNoCafRefs
  | HsCprInfo
  | HsWorker		name		-- Worker, if any

instance Outputable name => Outputable (HsIdInfo name) where
  ppr (HsUnfold _ unf) = ptext (SLIT("Unfolding:")) <+> ppr unf
  ppr other	       = empty	-- Havn't got around to this yet

data HsStrictnessInfo
  = HsStrictnessInfo ([Demand], Bool)
  | HsBottom
\end{code}

 
%************************************************************************
%*									*
\subsection{Rules in interface files}
%*									*
%************************************************************************

\begin{code}
data UfRuleBody name = UfRuleBody   FAST_STRING [UfBinder name] [UfExpr name] (UfExpr name)	-- Pre typecheck
		     | CoreRuleBody FAST_STRING [CoreBndr]      [CoreExpr]    CoreExpr		-- Post typecheck
\end{code}
