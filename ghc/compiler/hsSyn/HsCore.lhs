%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
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
	UfExpr(..), UfAlts(..), UfBinder(..), UfNote(..),
	UfDefault(..), UfBinding(..),
	UfArg(..), UfPrimOp(..)
    ) where

#include "HsVersions.h"

-- friends:
import HsTypes		( HsType, pprParendHsType )
import Kind		( Kind {- instance Outputable -} )

-- others:
import Literal		( Literal )
import Util		( panic )
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
  | UfLit	Literal
  | UfCon 	name [UfArg name]
  | UfPrim	(UfPrimOp name) [UfArg name]
  | UfLam 	(UfBinder name)	  (UfExpr name)
  | UfApp 	(UfExpr name) (UfArg name)
  | UfCase	(UfExpr name) (UfAlts name)
  | UfLet	(UfBinding name)  (UfExpr name)
  | UfNote	(UfNote name) (UfExpr name)

data UfPrimOp name
  = UfCCallOp	FAST_STRING	     -- callee
		Bool		     -- True <=> casm, rather than ccall
		Bool		     -- True <=> might cause GC
		[HsType name] -- arg types, incl state token
				     -- (which will be first)
		(HsType name) -- return type

  | UfOtherOp	name

data UfNote name = UfSCC CostCentre
	         | UfCoerce (HsType name)
	         | UfInlineCall

data UfAlts name
  = UfAlgAlts  [(name, [name], UfExpr name)]
		(UfDefault name)
  | UfPrimAlts [(Literal, UfExpr name)]
		(UfDefault name)

data UfDefault name
  = UfNoDefault
  | UfBindDefault name (UfExpr name)

data UfBinding name
  = UfNonRec	(UfBinder name)
		(UfExpr name)
  | UfRec 	[(UfBinder name, UfExpr name)]

data UfBinder name
  = UfValBinder	name (HsType name)
  | UfTyBinder	name Kind

data UfArg name
  = UfVarArg	name
  | UfLitArg	Literal
  | UfTyArg	(HsType name)
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

    ppr (UfCon c as)
      = hsep [text "UfCon", ppr c, ppr as, char ')']
    ppr (UfPrim o as)
      = hsep [text "UfPrim", ppr o, ppr as, char ')']

    ppr (UfLam b body)
      = hsep [char '\\', ppr b, ptext SLIT("->"), ppr body]

    ppr (UfApp fun (UfTyArg ty))
      = hsep [ppr fun, char '@', pprParendHsType ty]

    ppr (UfApp fun (UfLitArg lit))
      = hsep [ppr fun, ppr lit]

    ppr (UfApp fun (UfVarArg var))
      = hsep [ppr fun, ppr var]

    ppr (UfCase scrut alts)
      = hsep [ptext SLIT("case"), ppr scrut, ptext SLIT("of {"), pp_alts alts, char '}']
      where
    	pp_alts (UfAlgAlts alts deflt)
	  = hsep [hsep (punctuate semi (map pp_alt alts)), pp_deflt deflt]
	  where
	   pp_alt (c,bs,rhs) = hsep [ppr c, ppr bs, ppr_arrow, ppr rhs]
    	pp_alts (UfPrimAlts alts deflt)
	  = hsep [hsep (punctuate semi (map pp_alt alts)), pp_deflt deflt]
	  where
	   pp_alt (l,rhs) = hsep [ppr l, ppr_arrow, ppr rhs]

	pp_deflt UfNoDefault = empty
	pp_deflt (UfBindDefault b rhs) = hsep [ppr b, ppr_arrow, ppr rhs]

        ppr_arrow = ptext SLIT("->")

    ppr (UfLet (UfNonRec b rhs) body)
      = hsep [ptext SLIT("let"), ppr b, equals, ppr rhs, ptext SLIT("in"), ppr body]
    ppr (UfLet (UfRec pairs) body)
      = hsep [ptext SLIT("letrec"), braces (hsep (punctuate semi (map pp_pair pairs))), ptext SLIT("in"), ppr body]
      where
	pp_pair (b,rhs) = hsep [ppr b, equals, ppr rhs]

    ppr (UfNote note body)
      = hsep [ptext SLIT("_NOTE_ [ToDo]>"), ppr body]

instance Outputable name => Outputable (UfPrimOp name) where
    ppr (UfCCallOp str is_casm can_gc arg_tys result_ty)
      = let
	    before = ptext (if is_casm then SLIT("_casm_ ``") else SLIT("_ccall_ "))
	    after  = if is_casm then text "'' " else space
	in
	hcat [before, ptext str, after,
		   brackets (ppr arg_tys), space, ppr result_ty]

    ppr (UfOtherOp op)
      = ppr op

instance Outputable name => Outputable (UfArg name) where
    ppr (UfVarArg v)	= ppr v
    ppr (UfLitArg l)	= ppr l
    ppr (UfTyArg ty)	= pprParendHsType ty

instance Outputable name => Outputable (UfBinder name) where
    ppr (UfValBinder name ty)  = hsep [ppr name, ptext SLIT("::"), ppr ty]
    ppr (UfTyBinder name kind) = hsep [ppr name, ptext SLIT("::"), ppr kind]
\end{code}

