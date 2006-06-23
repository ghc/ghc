%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%

\begin{code}
module Kind (
	Kind(..), SimpleKind, 
	openTypeKind, liftedTypeKind, unliftedTypeKind, unboxedTypeKind,
	argTypeKind, ubxTupleKind,

	isLiftedTypeKind, isUnliftedTypeKind, isUnliftedBoxedTypeKind,
	isArgTypeKind, isOpenTypeKind,
	mkArrowKind, mkArrowKinds,

        isSubKind, defaultKind, 
	kindFunResult, splitKindFunTys, 

	KindVar, mkKindVar, kindVarRef, kindVarUniq, 
	kindVarOcc, setKindVarOcc,

	pprKind, pprParendKind
     ) where

#include "HsVersions.h"

import Unique	( Unique )
import OccName  ( OccName, mkOccName, tvName )
import Outputable
import DATA_IOREF
\end{code}

Kinds
~~~~~
There's a little subtyping at the kind level:  

		 ?
		/ \
	       /   \
	      ??   (#)
	    / | \
	   *  !  #

where	*    [LiftedTypeKind]   means boxed type
	#    [UnboxedTypeKind]  means unboxed type
	(#)  [UbxTupleKind]     means unboxed tuple
	??   [ArgTypeKind]      is the lub of *,#
	?    [OpenTypeKind]	means any type at all

In particular:

	error :: forall a:?. String -> a
	(->)  :: ?? -> ? -> *
	(\(x::t) -> ...)	Here t::?? (i.e. not unboxed tuple)

\begin{code}
data Kind 
  = LiftedTypeKind 	--  *
  | OpenTypeKind	--  ?
  | UnboxedTypeKind	--  #
  | UnliftedTypeKind    --  !
  | UbxTupleKind	--  (##)
  | ArgTypeKind		--  ??
  | FunKind Kind Kind	--  k1 -> k2
  | KindVar KindVar
  deriving( Eq )

data KindVar = KVar Unique OccName (IORef (Maybe SimpleKind))
  -- INVARIANT: a KindVar can only be instantiated by a SimpleKind

type SimpleKind = Kind	
  -- A SimpleKind has no ? or # kinds in it:
  -- sk ::= * | sk1 -> sk2 | kvar

instance Eq KindVar where
  (KVar u1 _ _) == (KVar u2 _ _) = u1 == u2

mkKindVar :: Unique -> IORef (Maybe Kind) -> KindVar
mkKindVar u r = KVar u kind_var_occ r

kindVarRef :: KindVar -> IORef (Maybe Kind)
kindVarRef (KVar _ _ ref) = ref

kindVarUniq :: KindVar -> Unique
kindVarUniq (KVar uniq _ _) = uniq

kindVarOcc :: KindVar -> OccName
kindVarOcc (KVar _ occ _) = occ

setKindVarOcc :: KindVar -> OccName -> KindVar
setKindVarOcc (KVar u _ r) occ = KVar u occ r

kind_var_occ :: OccName	-- Just one for all KindVars
			-- They may be jiggled by tidying
kind_var_occ = mkOccName tvName "k"
\end{code}

Kind inference
~~~~~~~~~~~~~~
During kind inference, a kind variable unifies only with 
a "simple kind", sk
	sk ::= * | sk1 -> sk2
For example 
	data T a = MkT a (T Int#)
fails.  We give T the kind (k -> *), and the kind variable k won't unify
with # (the kind of Int#).

Type inference
~~~~~~~~~~~~~~
When creating a fresh internal type variable, we give it a kind to express 
constraints on it.  E.g. in (\x->e) we make up a fresh type variable for x, 
with kind ??.  

During unification we only bind an internal type variable to a type
whose kind is lower in the sub-kind hierarchy than the kind of the tyvar.

When unifying two internal type variables, we collect their kind constraints by
finding the GLB of the two.  Since the partial order is a tree, they only
have a glb if one is a sub-kind of the other.  In that case, we bind the
less-informative one to the more informative one.  Neat, eh?


\begin{code}
liftedTypeKind   = LiftedTypeKind
unboxedTypeKind  = UnboxedTypeKind
unliftedTypeKind = UnliftedTypeKind
openTypeKind     = OpenTypeKind
argTypeKind      = ArgTypeKind
ubxTupleKind	 = UbxTupleKind

mkArrowKind :: Kind -> Kind -> Kind
mkArrowKind k1 k2 = k1 `FunKind` k2

mkArrowKinds :: [Kind] -> Kind -> Kind
mkArrowKinds arg_kinds result_kind = foldr mkArrowKind result_kind arg_kinds
\end{code}

%************************************************************************
%*									*
	Functions over Kinds		
%*									*
%************************************************************************

\begin{code}
kindFunResult :: Kind -> Kind
kindFunResult (FunKind _ k) = k
kindFunResult k = pprPanic "kindFunResult" (ppr k)

splitKindFunTys :: Kind -> ([Kind],Kind)
splitKindFunTys (FunKind k1 k2) = case splitKindFunTys k2 of
				    (as, r) -> (k1:as, r)
splitKindFunTys k = ([], k)

isLiftedTypeKind, isUnliftedTypeKind :: Kind -> Bool
isLiftedTypeKind LiftedTypeKind = True
isLiftedTypeKind other		= False

isUnliftedBoxedTypeKind UnliftedTypeKind = True
isUnliftedBoxedTypeKind other	    = False

isUnliftedTypeKind UnliftedTypeKind = True
isUnliftedTypeKind UnboxedTypeKind  = True
isUnliftedTypeKind other	    = False

isArgTypeKind :: Kind -> Bool
-- True of any sub-kind of ArgTypeKind 
isArgTypeKind LiftedTypeKind   = True
isArgTypeKind UnliftedTypeKind = True
isArgTypeKind UnboxedTypeKind  = True
isArgTypeKind ArgTypeKind      = True
isArgTypeKind other	       = False

isOpenTypeKind :: Kind -> Bool
-- True of any sub-kind of OpenTypeKind (i.e. anything except arrow)
isOpenTypeKind (FunKind _ _) = False
isOpenTypeKind (KindVar _)   = False	-- This is a conservative answer
					-- It matters in the call to isSubKind in
					-- checkExpectedKind.
isOpenTypeKind other	     = True

isSubKind :: Kind -> Kind -> Bool
-- (k1 `isSubKind` k2) checks that k1 <: k2
isSubKind LiftedTypeKind   LiftedTypeKind   = True
isSubKind UnliftedTypeKind UnliftedTypeKind = True
isSubKind UnboxedTypeKind  UnboxedTypeKind  = True
isSubKind UbxTupleKind     UbxTupleKind     = True
isSubKind k1 		   OpenTypeKind     = isOpenTypeKind k1
isSubKind k1 		   ArgTypeKind      = isArgTypeKind k1
isSubKind (FunKind a1 r1) (FunKind a2 r2)   = (a2 `isSubKind` a1) && (r1 `isSubKind` r2)
isSubKind k1		  k2		    = False

defaultKind :: Kind -> Kind
-- Used when generalising: default kind '?' and '??' to '*'
-- 
-- When we generalise, we make generic type variables whose kind is
-- simple (* or *->* etc).  So generic type variables (other than
-- built-in constants like 'error') always have simple kinds.  This is important;
-- consider
--	f x = True
-- We want f to get type
--	f :: forall (a::*). a -> Bool
-- Not 
--	f :: forall (a::??). a -> Bool
-- because that would allow a call like (f 3#) as well as (f True),
--and the calling conventions differ.  This defaulting is done in TcMType.zonkTcTyVarBndr.
defaultKind OpenTypeKind = LiftedTypeKind
defaultKind ArgTypeKind  = LiftedTypeKind
defaultKind kind	 = kind
\end{code}


%************************************************************************
%*									*
		Pretty printing
%*									*
%************************************************************************

\begin{code}
instance Outputable KindVar where
  ppr (KVar uniq occ _) = ppr occ <> ifPprDebug (ppr uniq)

instance Outputable Kind where
  ppr k = pprKind k

pprParendKind :: Kind -> SDoc
pprParendKind k@(FunKind _ _) = parens (pprKind k)
pprParendKind k		      = pprKind k

pprKind (KindVar v)      = ppr v
pprKind LiftedTypeKind   = ptext SLIT("*")
pprKind UnliftedTypeKind = ptext SLIT("!")
pprKind UnboxedTypeKind  = ptext SLIT("#")
pprKind OpenTypeKind     = ptext SLIT("?")
pprKind ArgTypeKind      = ptext SLIT("??")
pprKind UbxTupleKind     = ptext SLIT("(#)")
pprKind (FunKind k1 k2)  = sep [ pprParendKind k1, arrow <+> pprKind k2]

\end{code}
