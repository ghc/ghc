%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[HsTypes]{Abstract syntax: user-defined types}

\begin{code}
#include "HsVersions.h"

module HsTypes (
	PolyType(..), MonoType(..),
	ClassAssertion(..), Context(..),

	ProtoNameContext(..),
	ProtoNameMonoType(..),
	ProtoNamePolyType(..),
	RenamedContext(..),
	RenamedMonoType(..),
	RenamedPolyType(..),

	cmpPolyType, cmpMonoType, cmpList,
	eqMonoType,
	
	pprContext, pprParendMonoType

    ) where

import ProtoName
import Name 		( Name )
import Unique		( Unique )
import Outputable
import Pretty
import Util
\end{code}

This is the syntax for types as seen in type signatures.

\begin{code}
data PolyType name
  = UnoverloadedTy	(MonoType name) -- equiv to having a [] context

  | OverloadedTy	(Context name)	-- not supposed to be []
			(MonoType name)

  -- this next one is only used in unfoldings in interfaces
  | ForAllTy		[name]
			(MonoType name)

type Context name = [ClassAssertion name]

type ClassAssertion name = (name, name)

type ProtoNamePolyType = PolyType ProtoName
type RenamedPolyType   = PolyType Name

type ProtoNameContext  = Context  ProtoName
type RenamedContext    = Context  Name

data MonoType name
  = MonoTyVar		name		-- Type variable
  | MonoTyCon		name		-- Type constructor
			[MonoType name]
  | FunMonoTy		(MonoType name) -- function type
			(MonoType name)
  | ListMonoTy		(MonoType name) -- list type
  | TupleMonoTy		[PolyType name] -- tuple type (length gives arity)
	-- *** NOTA BENE *** The tuple type takes *Poly*Type
	-- arguments, because these *do* arise in pragmatic info
	-- in interfaces (mostly to do with dictionaries).  It just
	-- so happens that this won't happen for lists, etc.,
	-- (as far as I know).
	-- We might want to be less hacky about this in future. (ToDo)
	-- [WDP]

  -- these next two are only used in unfoldings in interfaces
  | MonoTyVarTemplate	name
  | MonoDict		name	-- Class
			(MonoType name)

#ifdef DPH
  | MonoTyProc		[MonoType name]
			(MonoType name)	-- Processor
  | MonoTyPod		(MonoType name) -- Pod
#endif {- Data Parallel Haskell -} 

type ProtoNameMonoType = MonoType ProtoName
type RenamedMonoType   = MonoType Name
\end{code}

We do define a specialised equality for these \tr{*Type} types; used
in checking interfaces.  Most any other use is likely to be {\em
wrong}, so be careful!
\begin{code}
cmpPolyType :: (a -> a -> TAG_) -> PolyType a -> PolyType a -> TAG_
cmpMonoType :: (a -> a -> TAG_) -> MonoType a -> MonoType a -> TAG_
cmpContext  :: (a -> a -> TAG_) -> Context  a -> Context  a -> TAG_
cmpList	    :: (a -> a -> TAG_) -> [a]	      -> [a]	    -> TAG_

cmpPolyType cmp (UnoverloadedTy  t1) (UnoverloadedTy  t2)
  = cmpMonoType cmp t1 t2
cmpPolyType cmp (OverloadedTy c1 t1) (OverloadedTy c2 t2)
  = case cmpContext cmp c1 c2 of { EQ_ -> cmpMonoType cmp t1 t2; xxx -> xxx }

cmpPolyType cmp (ForAllTy tvs1 t1) (ForAllTy tvs2 t2)
  = case cmp_tvs tvs1 tvs2 of { EQ_ -> cmpMonoType cmp t1 t2; xxx -> xxx }
  where
    cmp_tvs [] [] = EQ_
    cmp_tvs [] _  = LT_
    cmp_tvs _  [] = GT_
    cmp_tvs (a:as) (b:bs)
      = case cmp a b of { EQ_ -> cmp_tvs as bs; xxx -> xxx }
    cmp_tvs _ _ = case (panic "cmp_tvs") of { v -> cmp_tvs v v } -- BUG avoidance

cmpPolyType cmp ty1 ty2 -- tags must be different
  = let tag1 = tag ty1
	tag2 = tag ty2
    in
    if tag1 _LT_ tag2 then LT_ else GT_
  where
    tag (UnoverloadedTy _) = (ILIT(1) :: FAST_INT)
    tag (OverloadedTy _ _) = ILIT(2)
    tag (ForAllTy _ _)	   = ILIT(3)

-----------
cmpMonoType cmp (MonoTyVar n1) (MonoTyVar n2)
  = cmp n1 n2

cmpMonoType cmp (TupleMonoTy tys1) (TupleMonoTy tys2)
  = cmpList (cmpPolyType cmp) tys1 tys2
cmpMonoType cmp (ListMonoTy ty1) (ListMonoTy ty2)
  = cmpMonoType cmp ty1 ty2

cmpMonoType cmp (MonoTyCon tc1 tys1) (MonoTyCon tc2 tys2)
  = case cmp tc1 tc2 of { EQ_ -> cmpList (cmpMonoType cmp) tys1 tys2; xxx -> xxx }

cmpMonoType cmp (FunMonoTy a1 b1) (FunMonoTy a2 b2)
  = case cmpMonoType cmp a1 a2 of { EQ_ -> cmpMonoType cmp b1 b2; xxx -> xxx }

cmpMonoType cmp (MonoTyVarTemplate n1) (MonoTyVarTemplate n2)
  = cmp n1 n2
cmpMonoType cmp (MonoDict c1 ty1)   (MonoDict c2 ty2)
  = case cmp c1 c2 of { EQ_ -> cmpMonoType cmp ty1 ty2; xxx -> xxx }

#ifdef DPH
cmpMonoType cmp (MonoTyProc tys1 ty1) (MonoTyProc tys2 ty2)
  = case cmpList (cmpMonoType cmp) tys1 tys2 of { EQ_ -> cmpMonoType cmp ty1 ty2; xxx -> xxx }
cmpMonoType cmp (MonoTyPod ty1)	 (MonoTyPod ty2) = cmpMonoType cmp ty1 ty2
#endif {- Data Parallel Haskell -}

cmpMonoType cmp ty1 ty2 -- tags must be different
  = let tag1 = tag ty1
	tag2 = tag ty2
    in
    if tag1 _LT_ tag2 then LT_ else GT_
  where
    tag (MonoTyVar n1)		= (ILIT(1) :: FAST_INT)
    tag (TupleMonoTy tys1)	= ILIT(2)
    tag (ListMonoTy ty1)	= ILIT(3)
    tag (MonoTyCon tc1 tys1)	= ILIT(4)
    tag (FunMonoTy a1 b1)	= ILIT(5)
    tag (MonoTyVarTemplate n1)	= ILIT(6)
    tag (MonoDict c1 ty1)	= ILIT(7)
#ifdef DPH
    tag (MonoTyProc tys1 ty1)	= ILIT(8)
    tag (MonoTyPod ty1)		= ILIT(9)
#endif {- Data Parallel Haskell -}

-------------------
cmpContext cmp a b
  = cmpList cmp_ctxt a b
  where
    cmp_ctxt (c1, tv1) (c2, tv2)
      = case cmp c1 c2 of { EQ_ -> cmp tv1 tv2; xxx -> xxx }

-------------------
cmpList cmp []     [] = EQ_
cmpList cmp []     _  = LT_
cmpList cmp _      [] = GT_
cmpList cmp (a:as) (b:bs)
  = case cmp a b of { EQ_ -> cmpList cmp as bs; xxx -> xxx }

cmpList cmp _ _
  = case (panic "cmpList (HsTypes)") of { l -> cmpList cmp l l } -- BUG avoidance
\end{code}

\begin{code}
eqMonoType :: ProtoNameMonoType -> ProtoNameMonoType -> Bool

eqMonoType a b = case (cmpMonoType cmpProtoName a b) of { EQ_ -> True; _ -> False }
\end{code}

This is used in various places:
\begin{code}
pprContext :: (Outputable name) => PprStyle -> (Context name) -> Pretty

pprContext sty []	    = ppNil
pprContext sty [(clas, ty)] = ppCat [ppr sty clas, ppr sty ty, ppStr "=>"]
pprContext sty context
  = ppBesides [ppLparen,
	   ppInterleave ppComma (map pp_assert context),
	   ppRparen, ppStr " =>"]
  where
    pp_assert (clas, ty)
      = ppCat [ppr sty clas, ppr sty ty]
\end{code}

\begin{code}
instance (Outputable name) => Outputable (PolyType name) where
    ppr sty (UnoverloadedTy ty) = ppr sty ty
    ppr sty (OverloadedTy ctxt ty)
     = ppCat [pprContext sty ctxt, ppr sty ty]
    ppr sty (ForAllTy tvs ty)
     = ppBesides [ppStr "_forall_ ", interppSP sty tvs, ppStr " => ", ppr sty ty]

instance (Outputable name) => Outputable (MonoType name) where
    ppr = pprMonoType

pREC_TOP = (0 :: Int)
pREC_FUN = (1 :: Int)
pREC_CON = (2 :: Int)

-- printing works more-or-less as for UniTypes (in UniTyFuns)

pprMonoType, pprParendMonoType :: (Outputable name) => PprStyle -> MonoType name -> Pretty

pprMonoType sty ty  	 = ppr_mono_ty sty pREC_TOP ty
pprParendMonoType sty ty = ppr_mono_ty sty pREC_CON ty

ppr_mono_ty sty ctxt_prec (MonoTyVar name) = ppr sty name

ppr_mono_ty sty ctxt_prec (FunMonoTy ty1 ty2)
  = let p1 = ppr_mono_ty sty pREC_FUN ty1
	p2 = ppr_mono_ty sty pREC_TOP ty2
    in
    if ctxt_prec < pREC_FUN then -- no parens needed
	ppSep [p1, ppBeside (ppStr "-> ") p2]
    else
	ppSep [ppBeside ppLparen p1, ppBesides [ppStr "-> ", p2, ppRparen]]

ppr_mono_ty sty ctxt_prec (TupleMonoTy tys)
 = ppBesides [ppLparen, ppInterleave ppComma (map (ppr sty) tys), ppRparen]

ppr_mono_ty sty ctxt_prec (ListMonoTy ty)
 = ppBesides [ppLbrack, ppr_mono_ty sty pREC_TOP ty, ppRbrack]

ppr_mono_ty sty ctxt_prec (MonoTyCon tycon tys)
  = let pp_tycon = ppr sty tycon in
    if null tys then
	pp_tycon
    else if ctxt_prec < pREC_CON then -- no parens needed
	ppCat [pp_tycon, ppInterleave ppNil (map (ppr_mono_ty sty pREC_CON) tys)]
    else
	ppBesides [ ppLparen, pp_tycon, ppSP,
	       ppInterleave ppNil (map (ppr_mono_ty sty pREC_CON) tys), ppRparen ]

-- unfoldings only
ppr_mono_ty sty ctxt_prec (MonoTyVarTemplate tv) = ppr sty tv

ppr_mono_ty sty ctxt_prec (MonoDict clas ty)
  = ppBesides [ppStr "{{", ppr sty clas, ppSP, ppr_mono_ty sty ctxt_prec ty, ppStr "}}"]

#ifdef DPH
ppr_mono_ty sty ctxt_prec (MonoTyProc tys ty)
     = ppBesides [ppStr "(|", 
		  ppInterleave ppComma (map (ppr_mono_ty sty pREC_TOP) tys), 
		  ppSemi,
		  ppr_mono_ty sty pREC_TOP ty,
		  ppStr "|)"]

ppr_mono_ty sty ctxt_prec (MonoTyPod ty)
     = ppBesides [ppStr "<<", ppr_mono_ty sty pREC_TOP ty ,ppStr ">>"]

#endif {- Data Parallel Haskell -}
\end{code}
