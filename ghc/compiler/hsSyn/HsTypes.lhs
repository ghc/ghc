%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsTypes]{Abstract syntax: user-defined types}

\begin{code}
module HsTypes (
	HsType(..), MonoUsageAnn(..), HsTyVar(..),
	Context, ClassAssertion

	, mkHsForAllTy, mkHsUsForAllTy
	, getTyVarName, replaceTyVarName
	, pprParendHsType
	, pprForAll, pprContext, pprClassAssertion
	, cmpHsType, cmpHsTypes, cmpContext
    ) where

#include "HsVersions.h"

import Type		( Kind, UsageAnn(..) )
import PprType		( {- instance Outputable Kind -} )
import Outputable
import Util		( thenCmp, cmpList )
\end{code}

This is the syntax for types as seen in type signatures.

\begin{code}
type Context name = [ClassAssertion name]

type ClassAssertion name = (name, [HsType name])
	-- The type is usually a type variable, but it
	-- doesn't have to be when reading interface files

data HsType name
  = HsForAllTy		(Maybe [HsTyVar name])	-- Nothing for implicitly quantified signatures
			(Context name)
			(HsType name)

  | MonoTyVar		name		-- Type variable

  | MonoTyApp		(HsType name)
			(HsType name)

  | MonoFunTy		(HsType name) -- function type
			(HsType name)

  | MonoListTy		(HsType name)	-- Element type

  | MonoTupleTy		[HsType name]	-- Element types (length gives arity)
			Bool		-- boxed?

  -- these next two are only used in interfaces
  | MonoDictTy		name	-- Class
			[HsType name]

  | MonoUsgTy           (MonoUsageAnn name)
                        (HsType name)

  | MonoUsgForAllTy     name
                        (HsType name)

data MonoUsageAnn name
  = MonoUsOnce
  | MonoUsMany
  | MonoUsVar name
  

-- Combine adjacent for-alls. 
-- The following awkward situation can happen otherwise:
--	f :: forall a. ((Num a) => Int)
-- might generate HsForAll (Just [a]) [] (HsForAll Nothing [Num a] t)
-- Then a isn't discovered as ambiguous, and we abstract the AbsBinds wrt []
-- but the export list abstracts f wrt [a].  Disaster.
--
-- A valid type must have one for-all at the top of the type, or of the fn arg types

mkHsForAllTy (Just []) [] ty = ty	-- Explicit for-all with no tyvars
mkHsForAllTy mtvs1     [] (HsForAllTy mtvs2 ctxt ty) = HsForAllTy (mtvs1 `plus` mtvs2) ctxt ty
						     where
						       mtvs1       `plus` Nothing     = mtvs1
						       Nothing     `plus` mtvs2       = mtvs2 
						       (Just tvs1) `plus` (Just tvs2) = Just (tvs1 ++ tvs2)
mkHsForAllTy tvs ctxt ty = HsForAllTy tvs ctxt ty

mkHsUsForAllTy uvs ty = foldr (\ uv ty -> MonoUsgForAllTy uv ty)
                              ty uvs

data HsTyVar name
  = UserTyVar name
  | IfaceTyVar name Kind
	-- *** NOTA BENE *** A "monotype" in a pragma can have
	-- for-alls in it, (mostly to do with dictionaries).  These
	-- must be explicitly Kinded.

getTyVarName (UserTyVar n)    = n
getTyVarName (IfaceTyVar n _) = n

replaceTyVarName :: HsTyVar name1 -> name2 -> HsTyVar name2
replaceTyVarName (UserTyVar n)    n' = UserTyVar n'
replaceTyVarName (IfaceTyVar n k) n' = IfaceTyVar n' k
\end{code}


%************************************************************************
%*									*
\subsection{Pretty printing}
%*									*
%************************************************************************

\begin{code}

instance (Outputable name) => Outputable (HsType name) where
    ppr ty = pprHsType ty

instance (Outputable name) => Outputable (HsTyVar name) where
    ppr (UserTyVar name)       = ppr name
    ppr (IfaceTyVar name kind) = hsep [ppr name, dcolon, ppr kind]

-- Better to see those for-alls
-- pprForAll []  = empty
pprForAll tvs = ptext SLIT("forall") <+> interppSP tvs <> ptext SLIT(".")

pprContext :: (Outputable name) => Context name -> SDoc
pprContext []	   = empty
pprContext context = parens (hsep (punctuate comma (map pprClassAssertion context))) <+> ptext SLIT("=>")

pprClassAssertion :: (Outputable name) => ClassAssertion name -> SDoc
pprClassAssertion (clas, tys) 
  = ppr clas <+> hsep (map pprParendHsType tys)
\end{code}

\begin{code}
pREC_TOP = (0 :: Int)
pREC_FUN = (1 :: Int)
pREC_CON = (2 :: Int)

maybeParen :: Bool -> SDoc -> SDoc
maybeParen True  p = parens p
maybeParen False p = p
	
-- printing works more-or-less as for Types

pprHsType, pprParendHsType :: (Outputable name) => HsType name -> SDoc

pprHsType ty       = ppr_mono_ty pREC_TOP ty
pprParendHsType ty = ppr_mono_ty pREC_CON ty

ppr_mono_ty ctxt_prec (HsForAllTy maybe_tvs ctxt ty)
  = maybeParen (ctxt_prec >= pREC_FUN) $
    sep [pp_tvs, pprContext ctxt, pprHsType ty]
  where
    pp_tvs = case maybe_tvs of
		Just tvs -> pprForAll tvs
		Nothing  -> text "{- implicit forall -}"

ppr_mono_ty ctxt_prec (MonoTyVar name)
  = ppr name

ppr_mono_ty ctxt_prec (MonoFunTy ty1 ty2)
  = let p1 = ppr_mono_ty pREC_FUN ty1
	p2 = ppr_mono_ty pREC_TOP ty2
    in
    maybeParen (ctxt_prec >= pREC_FUN)
	       (sep [p1, (<>) (ptext SLIT("-> ")) p2])

ppr_mono_ty ctxt_prec (MonoTupleTy tys True)
 = parens (sep (punctuate comma (map ppr tys)))
ppr_mono_ty ctxt_prec (MonoTupleTy tys False)
 = ptext SLIT("(#") <> sep (punctuate comma (map ppr tys)) <> ptext SLIT("#)")

ppr_mono_ty ctxt_prec (MonoListTy ty)
 = brackets (ppr_mono_ty pREC_TOP ty)

ppr_mono_ty ctxt_prec (MonoTyApp fun_ty arg_ty)
  = maybeParen (ctxt_prec >= pREC_CON)
	       (hsep [ppr_mono_ty pREC_FUN fun_ty, ppr_mono_ty pREC_CON arg_ty])

ppr_mono_ty ctxt_prec (MonoDictTy clas tys)
  = ppr clas <+> hsep (map (ppr_mono_ty pREC_CON) tys)

ppr_mono_ty ctxt_prec ty@(MonoUsgForAllTy _ _)
  = maybeParen (ctxt_prec >= pREC_FUN) $
    sep [ ptext SLIT("__fuall") <+> brackets pp_uvars <+> ptext SLIT("=>"),
          ppr_mono_ty pREC_TOP sigma
        ]
  where
    (uvars,sigma) = split [] ty
    pp_uvars      = interppSP uvars

    split uvs (MonoUsgForAllTy uv ty') = split (uv:uvs) ty'
    split uvs ty'                      = (reverse uvs,ty')

ppr_mono_ty ctxt_prec (MonoUsgTy u ty)
  = maybeParen (ctxt_prec >= pREC_CON) $
    ptext SLIT("__u") <+> pp_ua <+> ppr_mono_ty pREC_CON ty
  where
    pp_ua = case u of
              MonoUsOnce   -> ptext SLIT("-")
              MonoUsMany   -> ptext SLIT("!")
              MonoUsVar uv -> ppr uv
\end{code}


%************************************************************************
%*									*
\subsection{Comparison}
%*									*
%************************************************************************

We do define a specialised equality for these \tr{*Type} types; used
in checking interfaces.  Most any other use is likely to be {\em
wrong}, so be careful!

\begin{code}
cmpHsTyVar  :: (a -> a -> Ordering) -> HsTyVar a  -> HsTyVar a  -> Ordering
cmpHsType   :: (a -> a -> Ordering) -> HsType a   -> HsType a   -> Ordering
cmpHsTypes  :: (a -> a -> Ordering) -> [HsType a] -> [HsType a] -> Ordering
cmpContext  :: (a -> a -> Ordering) -> Context  a -> Context  a -> Ordering

cmpHsTyVar cmp (UserTyVar v1)    (UserTyVar v2)    = v1 `cmp` v2
cmpHsTyVar cmp (IfaceTyVar v1 _) (IfaceTyVar v2 _) = v1 `cmp` v2
cmpHsTyVar cmp (UserTyVar _)	 other		   = LT
cmpHsTyVar cmp other1	 	 other2		   = GT


cmpHsTypes cmp [] []   = EQ
cmpHsTypes cmp [] tys2 = LT
cmpHsTypes cmp tys1 [] = GT
cmpHsTypes cmp (ty1:tys1) (ty2:tys2) = cmpHsType cmp ty1 ty2 `thenCmp` cmpHsTypes cmp tys1 tys2

cmpHsType cmp (HsForAllTy tvs1 c1 t1) (HsForAllTy tvs2 c2 t2)
  = cmpMaybe (cmpList (cmpHsTyVar cmp)) tvs1 tvs2	`thenCmp`
    cmpContext cmp c1 c2    				`thenCmp`
    cmpHsType cmp t1 t2

cmpHsType cmp (MonoTyVar n1) (MonoTyVar n2)
  = cmp n1 n2

cmpHsType cmp (MonoTupleTy tys1 b1) (MonoTupleTy tys2 b2)
  = (b1 `compare` b2) `thenCmp` cmpHsTypes cmp tys1 tys2

cmpHsType cmp (MonoListTy ty1) (MonoListTy ty2)
  = cmpHsType cmp ty1 ty2

cmpHsType cmp (MonoTyApp fun_ty1 arg_ty1) (MonoTyApp fun_ty2 arg_ty2)
  = cmpHsType cmp fun_ty1 fun_ty2 `thenCmp` cmpHsType cmp arg_ty1 arg_ty2

cmpHsType cmp (MonoFunTy a1 b1) (MonoFunTy a2 b2)
  = cmpHsType cmp a1 a2 `thenCmp` cmpHsType cmp b1 b2

cmpHsType cmp (MonoDictTy c1 tys1)   (MonoDictTy c2 tys2)
  = cmp c1 c2 `thenCmp` cmpHsTypes cmp tys1 tys2

cmpHsType cmp (MonoUsgTy u1 ty1) (MonoUsgTy u2 ty2)
  = cmpUsg cmp u1 u2 `thenCmp` cmpHsType cmp ty1 ty2

cmpHsType cmp ty1 ty2 -- tags must be different
  = let tag1 = tag ty1
	tag2 = tag ty2
    in
    if tag1 _LT_ tag2 then LT else GT
  where
    tag (MonoTyVar n1)			= (ILIT(1) :: FAST_INT)
    tag (MonoTupleTy tys1 _)		= ILIT(2)
    tag (MonoListTy ty1)		= ILIT(3)
    tag (MonoTyApp tc1 tys1)		= ILIT(4)
    tag (MonoFunTy a1 b1)		= ILIT(5)
    tag (MonoDictTy c1 tys1)		= ILIT(6)
    tag (MonoUsgTy c1 ty1)		= ILIT(7)
    tag (MonoUsgForAllTy uv1 ty1)       = ILIT(8)
    tag (HsForAllTy _ _ _)		= ILIT(9)

-------------------
cmpContext cmp a b
  = cmpList cmp_ctxt a b
  where
    cmp_ctxt (c1, tys1) (c2, tys2)
      = cmp c1 c2 `thenCmp` cmpHsTypes cmp tys1 tys2

cmpUsg cmp  MonoUsOnce     MonoUsOnce    = EQ
cmpUsg cmp  MonoUsMany     MonoUsMany    = EQ
cmpUsg cmp (MonoUsVar u1) (MonoUsVar u2) = cmp u1 u2

cmpUsg cmp ua1 ua2  -- tags must be different
  = let tag1 = tag ua1
        tag2 = tag ua2
    in
        if tag1 _LT_ tag2 then LT else GT
  where
    tag MonoUsOnce       = (ILIT(1) :: FAST_INT)
    tag MonoUsMany       = ILIT(2)
    tag (MonoUsVar    _) = ILIT(3)

-- Should be in Maybes, I guess
cmpMaybe cmp Nothing  Nothing  = EQ
cmpMaybe cmp Nothing  (Just x) = LT
cmpMaybe cmp (Just x)  Nothing = GT
cmpMaybe cmp (Just x) (Just y) = x `cmp` y
\end{code}
