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
	UfBinding(..), UfCon(..)
    ) where

#include "HsVersions.h"

-- friends:
import HsTypes		( HsType, pprParendHsType )

-- others:
import Const		( Literal )
import Type		( Kind )
import CostCentre
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
  | UfCon 	(UfCon name) [UfExpr name]
  | UfTuple 	name [UfExpr name]		-- Type arguments omitted
  | UfLam 	(UfBinder name)	  (UfExpr name)
  | UfApp 	(UfExpr name) (UfExpr name)
  | UfCase	(UfExpr name) name [UfAlt name]
  | UfLet	(UfBinding name)  (UfExpr name)
  | UfNote	(UfNote name) (UfExpr name)

data UfNote name = UfSCC CostCentre
	         | UfCoerce (HsType name)
	         | UfInlineCall

type UfAlt name = (UfCon name, [name], UfExpr name)

data UfCon name = UfDefault
 		| UfDataCon name
		| UfLitCon Literal
		| UfLitLitCon FAST_STRING (HsType name)
	        | UfPrimOp name
 	 	| UfCCallOp FAST_STRING	   -- callee
			    Bool	   -- True => dynamic (first arg is fun. pointer)
			    Bool	   -- True <=> casm, rather than ccall
			    Bool	   -- True <=> might cause GC

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
    ppr (UfType ty) = char '@' <+> pprParendHsType ty

    ppr (UfCon c as)
      = hsep [text "UfCon", ppr c, ppr as]

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

instance Outputable name => Outputable (UfCon name) where
    ppr UfDefault	= text "DEFAULT"
    ppr (UfDataCon d)	= ppr d
    ppr (UfPrimOp p)	= ppr p
    ppr (UfCCallOp str is_dyn is_casm can_gc)
      =	hcat [before, ptext str, after]
      where
	    before = (if is_dyn then ptext SLIT("_dyn_") else empty) <>
		     ptext (if is_casm then SLIT("_casm_ ``") else SLIT("_ccall_ "))
	    after  = if is_casm then text "'' " else space

instance Outputable name => Outputable (UfBinder name) where
    ppr (UfValBinder name ty)  = hsep [ppr name, dcolon, ppr ty]
    ppr (UfTyBinder name kind) = hsep [ppr name, dcolon, ppr kind]
\end{code}

