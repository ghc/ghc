%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsTypes]{Abstract syntax: user-defined types}

If compiled without \tr{#define COMPILING_GHC}, you get
(part of) a Haskell-abstract-syntax library.  With it,
you get part of GHC.

\begin{code}
#include "HsVersions.h"

module HsTypes (
	PolyType(..), MonoType(..),
	Context(..), ClassAssertion(..)

#ifdef COMPILING_GHC
	, pprParendPolyType
	, pprParendMonoType, pprContext
	, extractMonoTyNames, extractCtxtTyNames
	, cmpPolyType, cmpMonoType, cmpContext
#endif
    ) where

#ifdef COMPILING_GHC
import Ubiq

import Outputable	( interppSP, ifnotPprForUser )
import Pretty
import Type		( Kind )
import Util		( thenCmp, cmpList, isIn, panic# )

#endif {- COMPILING_GHC -}
\end{code}

This is the syntax for types as seen in type signatures.

\begin{code}
data PolyType name
  = HsPreForAllTy	(Context name)
			(MonoType name)

	-- The renamer turns HsPreForAllTys into HsForAllTys when they
	-- occur in signatures, to make the binding of variables
	-- explicit.  This distinction is made visible for
	-- non-COMPILING_GHC code, because you probably want to do the
	-- same thing.

  | HsForAllTy		[name]
			(Context name)
			(MonoType name)

type Context name = [ClassAssertion name]

type ClassAssertion name = (name, name)

data MonoType name
  = MonoTyVar		name		-- Type variable

  | MonoTyApp		name		-- Type constructor or variable
			[MonoType name]

    -- We *could* have a "MonoTyCon name" equiv to "MonoTyApp name []"
    -- (for efficiency, what?)  WDP 96/02/18

  | MonoFunTy		(MonoType name) -- function type
			(MonoType name)

  | MonoListTy		(MonoType name) -- list type
  | MonoTupleTy		[MonoType name] -- tuple type (length gives arity)

#ifdef COMPILING_GHC
  -- these next two are only used in unfoldings in interfaces
  | MonoDictTy		name	-- Class
			(MonoType name)

  | MonoForAllTy	[(name, Kind)]
			(MonoType name)
	-- *** NOTA BENE *** A "monotype" in a pragma can have
	-- for-alls in it, (mostly to do with dictionaries).  These
	-- must be explicitly Kinded.

#endif {- COMPILING_GHC -}
\end{code}

This is used in various places:
\begin{code}
#ifdef COMPILING_GHC
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
    ppr sty (HsPreForAllTy ctxt ty)
      = print_it sty ppNil ctxt ty
    ppr sty (HsForAllTy [] ctxt ty)
      = print_it sty ppNil ctxt ty
    ppr sty (HsForAllTy tvs ctxt ty)
      = print_it sty
	    (ppBesides [ppStr "_forall_ ", interppSP sty tvs, ppStr " => "])
	    ctxt ty

print_it sty pp_forall ctxt ty
  = ppCat [ifnotPprForUser sty pp_forall, -- print foralls unless PprForUser
	   pprContext sty ctxt, ppr sty ty]

pprParendPolyType :: Outputable name => PprStyle -> PolyType name -> Pretty
pprParendPolyType sty ty = ppr sty ty -- ToDo: more later

instance (Outputable name) => Outputable (MonoType name) where
    ppr = pprMonoType

pREC_TOP = (0 :: Int)
pREC_FUN = (1 :: Int)
pREC_CON = (2 :: Int)

-- printing works more-or-less as for Types

pprMonoType, pprParendMonoType :: (Outputable name) => PprStyle -> MonoType name -> Pretty

pprMonoType sty ty  	 = ppr_mono_ty sty pREC_TOP ty
pprParendMonoType sty ty = ppr_mono_ty sty pREC_CON ty

ppr_mono_ty sty ctxt_prec (MonoTyVar name) = ppr sty name

ppr_mono_ty sty ctxt_prec (MonoFunTy ty1 ty2)
  = let p1 = ppr_mono_ty sty pREC_FUN ty1
	p2 = ppr_mono_ty sty pREC_TOP ty2
    in
    if ctxt_prec < pREC_FUN then -- no parens needed
	ppSep [p1, ppBeside (ppStr "-> ") p2]
    else
	ppSep [ppBeside ppLparen p1, ppBesides [ppStr "-> ", p2, ppRparen]]

ppr_mono_ty sty ctxt_prec (MonoTupleTy tys)
 = ppBesides [ppLparen, ppInterleave ppComma (map (ppr sty) tys), ppRparen]

ppr_mono_ty sty ctxt_prec (MonoListTy ty)
 = ppBesides [ppLbrack, ppr_mono_ty sty pREC_TOP ty, ppRbrack]

ppr_mono_ty sty ctxt_prec (MonoTyApp tycon tys)
  = let pp_tycon = ppr sty tycon in
    if null tys then
	pp_tycon
    else if ctxt_prec < pREC_CON then -- no parens needed
	ppCat [pp_tycon, ppInterleave ppNil (map (ppr_mono_ty sty pREC_CON) tys)]
    else
	ppBesides [ ppLparen, pp_tycon, ppSP,
	       ppInterleave ppNil (map (ppr_mono_ty sty pREC_CON) tys), ppRparen ]

-- unfoldings only
ppr_mono_ty sty ctxt_prec (MonoDictTy clas ty)
  = ppBesides [ppStr "{{", ppr sty clas, ppSP, ppr_mono_ty sty ctxt_prec ty, ppStr "}}"]

#endif {- COMPILING_GHC -}
\end{code}

\begin{code}
#ifdef COMPILING_GHC

extractCtxtTyNames :: Eq name => Context  name -> [name]
extractMonoTyNames :: Eq name => (name -> Bool) -> MonoType name -> [name]

extractCtxtTyNames ctxt
  = foldr get [] ctxt
  where
    get (clas, tv) acc
      | tv `is_elem` acc = acc
      | otherwise        = tv : acc

    is_elem = isIn "extractCtxtTyNames"

extractMonoTyNames is_tyvar_name ty
  = get ty []
  where
    get (MonoTyApp con tys) acc = let
				     rest = foldr get acc tys
				  in
				  if is_tyvar_name con && not (con `is_elem` rest)
				  then con : rest
				  else rest
    get (MonoListTy ty)	    acc = get ty acc
    get (MonoFunTy ty1 ty2) acc = get ty1 (get ty2 acc)
    get (MonoDictTy _ ty)   acc = get ty acc
    get (MonoTupleTy tys)   acc = foldr get acc tys
    get (MonoTyVar tv)      acc
      | tv `is_elem` acc	= acc
      | otherwise		= tv : acc

    is_elem = isIn "extractMonoTyNames"

#endif {- COMPILING_GHC -}
\end{code}

We do define a specialised equality for these \tr{*Type} types; used
in checking interfaces.  Most any other use is likely to be {\em
wrong}, so be careful!
\begin{code}
#ifdef COMPILING_GHC

cmpPolyType :: (a -> a -> TAG_) -> PolyType a -> PolyType a -> TAG_
cmpMonoType :: (a -> a -> TAG_) -> MonoType a -> MonoType a -> TAG_
cmpContext  :: (a -> a -> TAG_) -> Context  a -> Context  a -> TAG_

-- We assume that HsPreForAllTys have been smashed by now.
# ifdef DEBUG
cmpPolyType _ (HsPreForAllTy _ _) _ = panic# "cmpPolyType:HsPreForAllTy:1st arg"
cmpPolyType _ _ (HsPreForAllTy _ _) = panic# "cmpPolyType:HsPreForAllTy:2nd arg"
# endif

cmpPolyType cmp (HsForAllTy tvs1 c1 t1) (HsForAllTy tvs2 c2 t2)
  = thenCmp (cmp_tvs tvs1 tvs2)
	    (thenCmp (cmpContext cmp c1 c2) (cmpMonoType cmp t1 t2))
  where
    cmp_tvs [] [] = EQ_
    cmp_tvs [] _  = LT_
    cmp_tvs _  [] = GT_
    cmp_tvs (a:as) (b:bs)
      = thenCmp (cmp a b) (cmp_tvs as bs)
    cmp_tvs _ _ = panic# "cmp_tvs"

-----------
cmpMonoType cmp (MonoTyVar n1) (MonoTyVar n2)
  = cmp n1 n2

cmpMonoType cmp (MonoTupleTy tys1) (MonoTupleTy tys2)
  = cmpList (cmpMonoType cmp) tys1 tys2
cmpMonoType cmp (MonoListTy ty1) (MonoListTy ty2)
  = cmpMonoType cmp ty1 ty2

cmpMonoType cmp (MonoTyApp tc1 tys1) (MonoTyApp tc2 tys2)
  = thenCmp (cmp tc1 tc2) (cmpList (cmpMonoType cmp) tys1 tys2)

cmpMonoType cmp (MonoFunTy a1 b1) (MonoFunTy a2 b2)
  = thenCmp (cmpMonoType cmp a1 a2) (cmpMonoType cmp b1 b2)

cmpMonoType cmp (MonoDictTy c1 ty1)   (MonoDictTy c2 ty2)
  = thenCmp (cmp c1 c2) (cmpMonoType cmp ty1 ty2)

cmpMonoType cmp ty1 ty2 -- tags must be different
  = let tag1 = tag ty1
	tag2 = tag ty2
    in
    if tag1 _LT_ tag2 then LT_ else GT_
  where
    tag (MonoTyVar n1)		= (ILIT(1) :: FAST_INT)
    tag (MonoTupleTy tys1)	= ILIT(2)
    tag (MonoListTy ty1)	= ILIT(3)
    tag (MonoTyApp tc1 tys1)	= ILIT(4)
    tag (MonoFunTy a1 b1)	= ILIT(5)
    tag (MonoDictTy c1 ty1)	= ILIT(7)

-------------------
cmpContext cmp a b
  = cmpList cmp_ctxt a b
  where
    cmp_ctxt (c1, tv1) (c2, tv2)
      = thenCmp (cmp c1 c2) (cmp tv1 tv2)

#endif {- COMPILING_GHC -}
\end{code}
