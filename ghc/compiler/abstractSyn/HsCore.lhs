%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1994
%
%************************************************************************
%*									*
\section[HsCore]{Core-syntax unfoldings in Haskell interface files}
%*									*
%************************************************************************

We could either use this, or parameterise @CoreExpr@ on @UniTypes@ and
@TyVars@ as well.  Currently trying the former.

\begin{code}
#include "HsVersions.h"

module HsCore (
	-- types:
	UnfoldingCoreExpr(..), UnfoldingCoreAlts(..),
	UnfoldingCoreDefault(..), UnfoldingCoreBinding(..),
	UnfoldingCoreAtom(..), UfId(..), UnfoldingType(..),
	UnfoldingPrimOp(..), UfCostCentre(..),

	-- function:
	eqUfExpr
    ) where

IMPORT_Trace

import AbsPrel		( PrimOp, PrimKind )
import AbsSynFuns	( cmpInstanceTypes )
import BasicLit		( BasicLit )
import HsTypes		-- ( cmpPolyType, PolyType(..), MonoType )
import Maybes		( Maybe(..) )
import Name 		( Name )
import Outputable	-- class for printing, forcing
import Pretty		-- pretty-printing utilities
import PrimOps		( tagOf_PrimOp -- HACK
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import ProtoName	( cmpProtoName, eqProtoName, ProtoName(..) ) -- .. for pragmas
import Util
\end{code}

%************************************************************************
%*									*
\subsection[HsCore-types]{Types for read/written Core unfoldings}
%*									*
%************************************************************************

\begin{code}
data UnfoldingCoreExpr name
  = UfCoVar 	(UfId name)
  | UfCoLit	BasicLit
  | UfCoCon 	name -- must be a "BoringUfId"...
		[UnfoldingType name]
		[UnfoldingCoreAtom name]
  | UfCoPrim	(UnfoldingPrimOp name)
		[UnfoldingType name]
		[UnfoldingCoreAtom name]
  | UfCoLam 	[UfBinder name]
		(UnfoldingCoreExpr name)
  | UfCoTyLam	name
		(UnfoldingCoreExpr name)
  | UfCoApp 	(UnfoldingCoreExpr name)
		(UnfoldingCoreAtom name)
  | UfCoTyApp	(UnfoldingCoreExpr name)
		(UnfoldingType name)
  | UfCoCase	(UnfoldingCoreExpr name)
		(UnfoldingCoreAlts name)
  | UfCoLet	(UnfoldingCoreBinding name)
		(UnfoldingCoreExpr name)
  | UfCoSCC	(UfCostCentre name)
		(UnfoldingCoreExpr name)

type ProtoNameCoreExpr = UnfoldingCoreExpr ProtoName

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
  | UfCoPrimAlts [(BasicLit, UnfoldingCoreExpr name)]
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
  | UfCoLitAtom	BasicLit

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
    ppr sty (UfCoVar v) = pprUfId sty v
    ppr sty (UfCoLit l) = ppr sty l

    ppr sty (UfCoCon c tys as)
      = ppCat [ppStr "(UfCoCon", ppr sty c, ppr sty tys, ppr sty as, ppStr ")"]
    ppr sty (UfCoPrim o tys as)
      = ppCat [ppStr "(UfCoPrim", ppr sty o, ppr sty tys, ppr sty as, ppStr ")"]

    ppr sty (UfCoLam bs body)
      = ppCat [ppChar '\\', ppr sty bs, ppStr "->", ppr sty body]
    ppr sty (UfCoTyLam tv body)
      = ppCat [ppStr "/\\", ppr sty tv, ppStr "->", ppr sty body]

    ppr sty (UfCoApp fun arg)
      = ppCat [ppStr "(UfCoApp", ppr sty fun, ppr sty arg, ppStr ")"]
    ppr sty (UfCoTyApp expr ty)
      = ppCat [ppStr "(UfCoTyApp", ppr sty expr, ppr sty ty, ppStr ")"]

    ppr sty (UfCoCase scrut alts)
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

    ppr sty (UfCoLet (UfCoNonRec b rhs) body)
      = ppCat [ppStr "let", ppr sty b, ppEquals, ppr sty rhs, ppStr "in", ppr sty body]
    ppr sty (UfCoLet (UfCoRec pairs) body)
      = ppCat [ppStr "letrec {", ppInterleave ppSemi (map pp_pair pairs), ppStr "} in", ppr sty body]
      where
	pp_pair (b,rhs) = ppCat [ppr sty b, ppEquals, ppr sty rhs]

    ppr sty (UfCoSCC uf_cc body)
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

%************************************************************************
%*									*
\subsection[HsCore-equality]{Comparing Core unfoldings}
%*									*
%************************************************************************

We want to check that they are {\em exactly} the same.

\begin{code}
eqUfExpr :: ProtoNameCoreExpr -> ProtoNameCoreExpr -> Bool

eqUfExpr (UfCoVar v1)     (UfCoVar v2)     = eqUfId v1 v2
eqUfExpr (UfCoLit l1) (UfCoLit l2) = l1 == l2

eqUfExpr (UfCoCon c1 tys1 as1) (UfCoCon c2 tys2 as2)
  = eq_name c1 c2 && eq_lists eq_type tys1 tys2 && eq_lists eq_atom as1 as2
eqUfExpr (UfCoPrim o1 tys1 as1) (UfCoPrim o2 tys2 as2)
  = eq_op o1 o2 && eq_lists eq_type tys1 tys2 && eq_lists eq_atom as1 as2
  where
    eq_op (UfCCallOp _ _ _ _ _) (UfCCallOp _ _ _ _ _) = True
    eq_op (UfOtherOp o1)        (UfOtherOp o2)
      = tagOf_PrimOp o1 _EQ_ tagOf_PrimOp o2

eqUfExpr (UfCoLam bs1 body1) (UfCoLam bs2 body2)
  = eq_lists eq_binder bs1 bs2 && eqUfExpr body1 body2
eqUfExpr (UfCoTyLam tv1 body1) (UfCoTyLam tv2 body2)
  = eq_name tv1 tv2 && eqUfExpr body1 body2

eqUfExpr (UfCoApp fun1 arg1) (UfCoApp fun2 arg2)
  = eqUfExpr fun1 fun2 && eq_atom arg1 arg2
eqUfExpr (UfCoTyApp expr1 ty1) (UfCoTyApp expr2 ty2)
  = eqUfExpr expr1 expr2 && eq_type ty1 ty2

eqUfExpr (UfCoCase scrut1 alts1) (UfCoCase scrut2 alts2)
  = eqUfExpr scrut1 scrut2 && eq_alts alts1 alts2
  where
    eq_alts (UfCoAlgAlts alts1 deflt1) (UfCoAlgAlts alts2 deflt2)
      = eq_lists eq_alt alts1 alts2 && eq_deflt deflt1 deflt2
      where
       eq_alt (c1,bs1,rhs1) (c2,bs2,rhs2)
         = eq_name c1 c2 && eq_lists eq_binder bs1 bs2 && eqUfExpr rhs1 rhs2

    eq_alts (UfCoPrimAlts alts1 deflt1) (UfCoPrimAlts alts2 deflt2)
      = eq_lists eq_alt alts1 alts2 && eq_deflt deflt1 deflt2
      where
       eq_alt (l1,rhs1) (l2,rhs2)
         = l1 == l2 && eqUfExpr rhs1 rhs2

    eq_alts _ _ = False -- catch-all

    eq_deflt UfCoNoDefault UfCoNoDefault = True
    eq_deflt (UfCoBindDefault b1 rhs1) (UfCoBindDefault b2 rhs2)
      = eq_binder b1 b2 && eqUfExpr rhs1 rhs2
    eq_deflt _ _ = False

eqUfExpr (UfCoLet (UfCoNonRec b1 rhs1) body1) (UfCoLet (UfCoNonRec b2 rhs2) body2)
  = eq_binder b1 b2 && eqUfExpr rhs1 rhs2 && eqUfExpr body1 body2

eqUfExpr (UfCoLet (UfCoRec pairs1) body1) (UfCoLet (UfCoRec pairs2) body2)
  = eq_lists eq_pair pairs1 pairs2 && eqUfExpr body1 body2
  where
    eq_pair (b1,rhs1) (b2,rhs2) = eq_binder b1 b2 && eqUfExpr rhs1 rhs2

eqUfExpr (UfCoSCC cc1 body1) (UfCoSCC cc2 body2)
  = {-trace "eqUfExpr: not comparing cost-centres!"-} (eqUfExpr body1 body2)

eqUfExpr _ _ = False -- Catch-all
\end{code}

\begin{code}
eqUfId (BoringUfId n1) (BoringUfId n2)
  = eq_name n1 n2
eqUfId (SuperDictSelUfId a1 b1) (SuperDictSelUfId a2 b2)
  = eq_name a1 a2 && eq_name b1 b2
eqUfId (ClassOpUfId a1 b1) (ClassOpUfId a2 b2)
  = eq_name a1 a2 && eq_name b1 b2
eqUfId (DictFunUfId c1 t1) (DictFunUfId c2 t2)
  = eq_name c1 c2 && eq_tycon t1 t2 -- NB: **** only compare TyCons ******
  where
    eq_tycon (UnoverloadedTy ty1) (UnoverloadedTy ty2)
      = case (cmpInstanceTypes ty1 ty2) of { EQ_ -> True; _ -> False }
    eq_tycon ty1 ty2
      = trace "eq_tycon" (eq_type ty1 ty2) -- desperately try something else

eqUfId (ConstMethodUfId	a1 b1 t1) (ConstMethodUfId a2 b2 t2)
  = eq_name a1 a2 && eq_name b1 b2 && eq_type t1 t2
eqUfId (DefaultMethodUfId a1 b1) (DefaultMethodUfId a2 b2)
  = eq_name a1 a2 && eq_name b1 b2
eqUfId (SpecUfId id1 tms1) (SpecUfId id2 tms2)
  = eqUfId id1 id2 && eq_lists eq_ty_maybe tms1 tms2
  where
    eq_ty_maybe Nothing Nothing = True
    eq_ty_maybe (Just ty1) (Just ty2)
      = eq_type (UnoverloadedTy ty1) (UnoverloadedTy ty2)
      -- a HACKy way to compare MonoTypes (ToDo) [WDP 94/05/02]
    eq_ty_maybe _ _ = False
eqUfId (WorkerUfId id1) (WorkerUfId id2)
  = eqUfId id1 id2
eqUfId _ _ = False -- catch-all
\end{code}

\begin{code}
eq_atom (UfCoVarAtom id1) (UfCoVarAtom id2) = eqUfId id1 id2
eq_atom (UfCoLitAtom l1) (UfCoLitAtom l2) = l1 == l2
eq_atom _ _ = False

eq_binder (n1, ty1) (n2, ty2) = eq_name n1 n2 && eq_type ty1 ty2

eq_name :: ProtoName -> ProtoName -> Bool
eq_name pn1 pn2 = eqProtoName pn1 pn2 -- uses original names

eq_type ty1 ty2
  = case (cmpPolyType cmpProtoName ty1 ty2) of { EQ_ -> True; _ -> False }
\end{code}

\begin{code}
eq_lists :: (a -> a -> Bool) -> [a] -> [a] -> Bool

eq_lists eq [] [] = True
eq_lists eq [] _  = False
eq_lists eq _  [] = False
eq_lists eq (x:xs) (y:ys) = eq x y && eq_lists eq xs ys
\end{code}
