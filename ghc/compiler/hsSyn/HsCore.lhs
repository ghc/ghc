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
#include "HsVersions.h"

module HsCore (
	UfExpr(..), UfAlts(..), UfBinder(..), UfCoercion(..),
	UfDefault(..), UfBinding(..),
	UfArg(..), UfPrimOp(..)
    ) where

IMP_Ubiq()

-- friends:
import HsTypes		( HsType, pprParendHsType )
import PrimOp		( PrimOp, tagOf_PrimOp )
import Kind		( Kind {- instance Outputable -} )
import Type		( GenType {- instance Outputable -} )

-- others:
import Literal		( Literal )
import Outputable	( Outputable(..) )
import Pretty
import Util		( panic )
#if __GLASGOW_HASKELL__ >= 202
import CostCentre
#endif
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
  | UfSCC	CostCentre (UfExpr name)
  | UfCoerce	(UfCoercion name) (HsType name) (UfExpr name)

data UfPrimOp name
  = UfCCallOp	FAST_STRING	     -- callee
		Bool		     -- True <=> casm, rather than ccall
		Bool		     -- True <=> might cause GC
		[HsType name] -- arg types, incl state token
				     -- (which will be first)
		(HsType name) -- return type

  | UfOtherOp	name

data UfCoercion name = UfIn name | UfOut name

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
  | UfUsageBinder name

data UfArg name
  = UfVarArg	name
  | UfLitArg	Literal
  | UfTyArg	(HsType name)
  | UfUsageArg	name
\end{code}

%************************************************************************
%*									*
\subsection[HsCore-print]{Printing Core unfoldings}
%*									*
%************************************************************************

\begin{code}
instance Outputable name => Outputable (UfExpr name) where
    ppr sty (UfVar v) = ppr sty v
    ppr sty (UfLit l) = ppr sty l

    ppr sty (UfCon c as)
      = hsep [text "UfCon", ppr sty c, ppr sty as, char ')']
    ppr sty (UfPrim o as)
      = hsep [text "UfPrim", ppr sty o, ppr sty as, char ')']

    ppr sty (UfLam b body)
      = hsep [char '\\', ppr sty b, ptext SLIT("->"), ppr sty body]

    ppr sty (UfApp fun (UfTyArg ty))
      = hsep [ppr sty fun, char '@', pprParendHsType sty ty]

    ppr sty (UfApp fun (UfLitArg lit))
      = hsep [ppr sty fun, ppr sty lit]

    ppr sty (UfApp fun (UfVarArg var))
      = hsep [ppr sty fun, ppr sty var]

    ppr sty (UfCase scrut alts)
      = hsep [ptext SLIT("case"), ppr sty scrut, ptext SLIT("of {"), pp_alts alts, char '}']
      where
    	pp_alts (UfAlgAlts alts deflt)
	  = hsep [hsep (punctuate semi (map pp_alt alts)), pp_deflt deflt]
	  where
	   pp_alt (c,bs,rhs) = hsep [ppr sty c, ppr sty bs, ppr_arrow, ppr sty rhs]
    	pp_alts (UfPrimAlts alts deflt)
	  = hsep [hsep (punctuate semi (map pp_alt alts)), pp_deflt deflt]
	  where
	   pp_alt (l,rhs) = hsep [ppr sty l, ppr_arrow, ppr sty rhs]

	pp_deflt UfNoDefault = empty
	pp_deflt (UfBindDefault b rhs) = hsep [ppr sty b, ppr_arrow, ppr sty rhs]

        ppr_arrow = ptext SLIT("->")

    ppr sty (UfLet (UfNonRec b rhs) body)
      = hsep [ptext SLIT("let"), ppr sty b, equals, ppr sty rhs, ptext SLIT("in"), ppr sty body]
    ppr sty (UfLet (UfRec pairs) body)
      = hsep [ptext SLIT("letrec"), braces (hsep (punctuate semi (map pp_pair pairs))), ptext SLIT("in"), ppr sty body]
      where
	pp_pair (b,rhs) = hsep [ppr sty b, equals, ppr sty rhs]

    ppr sty (UfSCC uf_cc body)
      = hsep [ptext SLIT("_scc_ <cost-centre[ToDo]>"), ppr sty body]

instance Outputable name => Outputable (UfPrimOp name) where
    ppr sty (UfCCallOp str is_casm can_gc arg_tys result_ty)
      = let
	    before = ptext (if is_casm then SLIT("_casm_ ``") else SLIT("_ccall_ "))
	    after  = if is_casm then text "'' " else space
	in
	hcat [before, ptext str, after,
		   brackets (ppr sty arg_tys), space, ppr sty result_ty]

    ppr sty (UfOtherOp op)
      = ppr sty op

instance Outputable name => Outputable (UfArg name) where
    ppr sty (UfVarArg v)	= ppr sty v
    ppr sty (UfLitArg l)	= ppr sty l
    ppr sty (UfTyArg ty)	= pprParendHsType sty ty
    ppr sty (UfUsageArg name)	= ppr sty name

instance Outputable name => Outputable (UfBinder name) where
    ppr sty (UfValBinder name ty)  = hsep [ppr sty name, ptext SLIT("::"), ppr sty ty]
    ppr sty (UfTyBinder name kind) = hsep [ppr sty name, ptext SLIT("::"), ppr sty kind]
    ppr sty (UfUsageBinder name)   = ppr sty name
\end{code}

