%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
%************************************************************************
%*									*
\section[HsCore]{Core-syntax unfoldings in Haskell interface files}
%*									*
%************************************************************************

We could either use this, or parameterise @GenCoreExpr@ on @Types@ and
@TyVars@ as well.  Currently trying the former.

\begin{code}
#include "HsVersions.h"

module HsCore (
	UnfoldingCoreExpr(..), UnfoldingCoreAlts(..),
	UnfoldingCoreDefault(..), UnfoldingCoreBinding(..),
	UnfoldingCoreAtom(..), UfId(..), UnfoldingType(..),
	UnfoldingPrimOp(..), UfCostCentre(..)
    ) where

IMP_Ubiq()

-- friends:
import HsTypes		( MonoType, PolyType )
import PrimOp		( PrimOp, tagOf_PrimOp )

-- others:
import Literal		( Literal )
import Outputable	( Outputable(..) )
import Pretty
import Util		( panic )
\end{code}

%************************************************************************
%*									*
\subsection[HsCore-types]{Types for read/written Core unfoldings}
%*									*
%************************************************************************

\begin{code}
data UnfoldingCoreExpr name
  = UfVar 	(UfId name)
  | UfLit	Literal
  | UfCon 	name -- must be a "BoringUfId"...
		[UnfoldingType name]
		[UnfoldingCoreAtom name]
  | UfPrim	(UnfoldingPrimOp name)
		[UnfoldingType name]
		[UnfoldingCoreAtom name]
  | UfLam 	(UfBinder name)
		(UnfoldingCoreExpr name)
  | UfApp 	(UnfoldingCoreExpr name)
		(UnfoldingCoreAtom name)
  | UfCase	(UnfoldingCoreExpr name)
		(UnfoldingCoreAlts name)
  | UfLet	(UnfoldingCoreBinding name)
		(UnfoldingCoreExpr name)
  | UfSCC	(UfCostCentre name)
		(UnfoldingCoreExpr name)

data UnfoldingPrimOp name
  = UfCCallOp	FAST_STRING	     -- callee
		Bool		     -- True <=> casm, rather than ccall
		Bool		     -- True <=> might cause GC
		[UnfoldingType name] -- arg types, incl state token
				     -- (which will be first)
		(UnfoldingType name) -- return type
  | UfOtherOp	PrimOp

data UnfoldingCoreAlts name
  = UfCoAlgAlts	 [(name, [UfBinder name], UnfoldingCoreExpr name)]
		 (UnfoldingCoreDefault name)
  | UfCoPrimAlts [(Literal, UnfoldingCoreExpr name)]
		 (UnfoldingCoreDefault name)

data UnfoldingCoreDefault name
  = UfCoNoDefault
  | UfCoBindDefault (UfBinder name)
		    (UnfoldingCoreExpr name)

data UnfoldingCoreBinding name
  = UfCoNonRec	(UfBinder name)
		(UnfoldingCoreExpr name)
  | UfCoRec 	[(UfBinder name, UnfoldingCoreExpr name)]

data UnfoldingCoreAtom name
  = UfCoVarAtom	(UfId name)
  | UfCoLitAtom	Literal

data UfCostCentre name
  = UfPreludeDictsCC
		Bool	-- True <=> is dupd
  | UfAllDictsCC FAST_STRING	-- module and group
		FAST_STRING
		Bool	-- True <=> is dupd
  | UfUserCC	FAST_STRING
		FAST_STRING FAST_STRING -- module and group
		Bool	-- True <=> is dupd
		Bool	-- True <=> is CAF
  | UfAutoCC	(UfId name)
		FAST_STRING FAST_STRING -- module and group
		Bool Bool -- as above
  | UfDictCC	(UfId name)
		FAST_STRING FAST_STRING -- module and group
		Bool Bool -- as above

type UfBinder name = (name, UnfoldingType name)

data UfId name
  = BoringUfId		name
  | SuperDictSelUfId	name name	-- class and superclass
  | ClassOpUfId		name name	-- class and class op
  | DictFunUfId		name		-- class and type
			(UnfoldingType name)
  | ConstMethodUfId	name name	-- class, class op, and type
			(UnfoldingType name)
  | DefaultMethodUfId	name name	-- class and class op
  | SpecUfId		(UfId name)	-- its unspecialised "parent"
			[Maybe (MonoType name)]
  | WorkerUfId		(UfId name)	-- its non-working "parent"
  -- more to come?

type UnfoldingType name = PolyType name
\end{code}

%************************************************************************
%*									*
\subsection[HsCore-print]{Printing Core unfoldings}
%*									*
%************************************************************************

\begin{code}
instance Outputable name => Outputable (UnfoldingCoreExpr name) where
    ppr sty (UfVar v) = pprUfId sty v
    ppr sty (UfLit l) = ppr sty l

    ppr sty (UfCon c tys as)
      = ppCat [ppStr "(UfCon", ppr sty c, ppr sty tys, ppr sty as, ppStr ")"]
    ppr sty (UfPrim o tys as)
      = ppCat [ppStr "(UfPrim", ppr sty o, ppr sty tys, ppr sty as, ppStr ")"]

    ppr sty (UfLam bs body)
      = ppCat [ppChar '\\', ppr sty bs, ppStr "->", ppr sty body]

    ppr sty (UfApp fun arg)
      = ppCat [ppStr "(UfApp", ppr sty fun, ppr sty arg, ppStr ")"]

    ppr sty (UfCase scrut alts)
      = ppCat [ppStr "case", ppr sty scrut, ppStr "of {", pp_alts alts, ppStr "}"]
      where
    	pp_alts (UfCoAlgAlts alts deflt)
	  = ppCat [ppInterleave ppSemi (map pp_alt alts), pp_deflt deflt]
	  where
	   pp_alt (c,bs,rhs) = ppCat [ppr sty c, ppr sty bs, ppStr "->", ppr sty rhs]
    	pp_alts (UfCoPrimAlts alts deflt)
	  = ppCat [ppInterleave ppSemi (map pp_alt alts), pp_deflt deflt]
	  where
	   pp_alt (l,rhs) = ppCat [ppr sty l, ppStr "->", ppr sty rhs]

	pp_deflt UfCoNoDefault = ppNil
	pp_deflt (UfCoBindDefault b rhs) = ppCat [ppr sty b, ppStr "->", ppr sty rhs]

    ppr sty (UfLet (UfCoNonRec b rhs) body)
      = ppCat [ppStr "let", ppr sty b, ppEquals, ppr sty rhs, ppStr "in", ppr sty body]
    ppr sty (UfLet (UfCoRec pairs) body)
      = ppCat [ppStr "letrec {", ppInterleave ppSemi (map pp_pair pairs), ppStr "} in", ppr sty body]
      where
	pp_pair (b,rhs) = ppCat [ppr sty b, ppEquals, ppr sty rhs]

    ppr sty (UfSCC uf_cc body)
      = ppCat [ppStr "_scc_ <cost-centre[ToDo]>", ppr sty body]

instance Outputable name => Outputable (UnfoldingPrimOp name) where
    ppr sty (UfCCallOp str is_casm can_gc arg_tys result_ty)
      = let
	    before = ppStr (if is_casm then "_casm_ ``" else "_ccall_ ")
	    after  = if is_casm then ppStr "'' " else ppSP
	in
	ppBesides [before, ppPStr str, after,
		ppLbrack, ppr sty arg_tys, ppRbrack, ppSP, ppr sty result_ty]
    ppr sty (UfOtherOp op)
      = ppr sty op

instance Outputable name => Outputable (UnfoldingCoreAtom name) where
    ppr sty (UfCoVarAtom v) = pprUfId sty v
    ppr sty (UfCoLitAtom l)	    = ppr sty l

pprUfId sty (BoringUfId v) = ppr sty v
pprUfId sty (SuperDictSelUfId c sc)
  = ppBesides [ppStr "({-superdict-}", ppr sty c, ppSP, ppr sty sc, ppStr ")"]
pprUfId sty (ClassOpUfId c op)
  = ppBesides [ppStr "({-method-}", ppr sty c, ppSP, ppr sty op, ppStr ")"]
pprUfId sty (DictFunUfId c ty)
  = ppBesides [ppStr "({-dfun-}", ppr sty c, ppSP, ppr sty ty, ppStr ")"]
pprUfId sty (ConstMethodUfId c op ty)
  = ppBesides [ppStr "({-constm-}", ppr sty c, ppSP, ppr sty op, ppSP, ppr sty ty, ppStr ")"]
pprUfId sty (DefaultMethodUfId c ty)
  = ppBesides [ppStr "({-defm-}", ppr sty c, ppSP, ppr sty ty, ppStr ")"]

pprUfId sty (SpecUfId unspec ty_maybes)
  = ppBesides [ppStr "({-spec-} ", pprUfId sty unspec,
		ppInterleave ppSP (map pp_ty_maybe ty_maybes), ppStr ")"]
  where
    pp_ty_maybe Nothing  = ppStr "_N_"
    pp_ty_maybe (Just t) = ppr sty t

pprUfId sty (WorkerUfId unwrkr)
  = ppBesides [ppStr "({-wrkr-}", pprUfId sty unwrkr, ppStr ")"]
\end{code}

