{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module GHC.Types.Var.ArgFlag
  ( ArgFlag(Invisible,Required,Specified,Inferred)
  , Specificity(..)
  , isVisibleArgFlag
  , isInvisibleArgFlag
  , isInferredArgFlag
  , sameVis
  , AnonArgFlag(..)
  ) where

import GHC.Prelude

import GHC.Utils.Binary
import GHC.Utils.Outputable

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Data
import qualified Data.Semigroup as S

-- | Argument Flag
--
-- Is something required to appear in source Haskell ('Required'),
-- permitted by request ('Specified') (visible type application), or
-- prohibited entirely from appearing in source Haskell ('Inferred')?
-- See Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility] in "GHC.Core.TyCo.Rep

data ArgFlag = Invisible Specificity
             | Required
  deriving (Eq, Ord, Data)
  -- (<) on ArgFlag means "is less visible than"

-- | Whether an 'Invisible' argument may appear in source Haskell.
data Specificity = InferredSpec
                   -- ^ the argument may not appear in source Haskell, it is
                   -- only inferred.
                 | SpecifiedSpec
                   -- ^ the argument may appear in source Haskell, but isn't
                   -- required.
  deriving (Eq, Ord, Data)

pattern Inferred, Specified :: ArgFlag
pattern Inferred  = Invisible InferredSpec
pattern Specified = Invisible SpecifiedSpec

{-# COMPLETE Required, Specified, Inferred #-}

-- | Does this 'ArgFlag' classify an argument that is written in Haskell?
isVisibleArgFlag :: ArgFlag -> Bool
isVisibleArgFlag af = not (isInvisibleArgFlag af)

-- | Does this 'ArgFlag' classify an argument that is not written in Haskell?
isInvisibleArgFlag :: ArgFlag -> Bool
isInvisibleArgFlag (Invisible {}) = True
isInvisibleArgFlag Required       = False

isInferredArgFlag :: ArgFlag -> Bool
-- More restrictive than isInvisibleArgFlag
isInferredArgFlag (Invisible InferredSpec) = True
isInferredArgFlag _                        = False

-- | Do these denote the same level of visibility? 'Required'
-- arguments are visible, others are not. So this function
-- equates 'Specified' and 'Inferred'. Used for printing.
sameVis :: ArgFlag -> ArgFlag -> Bool
sameVis Required      Required      = True
sameVis (Invisible _) (Invisible _) = True
sameVis _             _             = False

instance Outputable ArgFlag where
  ppr Required  = text "[req]"
  ppr Specified = text "[spec]"
  ppr Inferred  = text "[infrd]"

instance Binary Specificity where
  put_ bh SpecifiedSpec = putByte bh 0
  put_ bh InferredSpec  = putByte bh 1

  get bh = do
    h <- getByte bh
    case h of
      0 -> return SpecifiedSpec
      _ -> return InferredSpec

instance Binary ArgFlag where
  put_ bh Required  = putByte bh 0
  put_ bh Specified = putByte bh 1
  put_ bh Inferred  = putByte bh 2

  get bh = do
    h <- getByte bh
    case h of
      0 -> return Required
      1 -> return Specified
      _ -> return Inferred

-- | The non-dependent version of 'ArgFlag'.
-- See Note [AnonArgFlag]
-- Appears here partly so that it's together with its friends ArgFlag
-- and ForallVisFlag, but also because it is used in IfaceType, rather
-- early in the compilation chain
data AnonArgFlag
  = VisArg    -- ^ Used for @(->)@: an ordinary non-dependent arrow.
              --   The argument is visible in source code.
  | InvisArg  -- ^ Used for @(=>)@: a non-dependent predicate arrow.
              --   The argument is invisible in source code.
  deriving (Eq, Ord, Data)

instance Outputable AnonArgFlag where
  ppr VisArg   = text "[vis]"
  ppr InvisArg = text "[invis]"

instance Binary AnonArgFlag where
  put_ bh VisArg   = putByte bh 0
  put_ bh InvisArg = putByte bh 1

  get bh = do
    h <- getByte bh
    case h of
      0 -> return VisArg
      _ -> return InvisArg

{- Note [AnonArgFlag]
~~~~~~~~~~~~~~~~~~~~~
AnonArgFlag is used principally in the FunTy constructor of Type.
  FunTy VisArg   t1 t2   means   t1 -> t2
  FunTy InvisArg t1 t2   means   t1 => t2

However, the AnonArgFlag in a FunTy is just redundant, cached
information.  In (FunTy { ft_af = af, ft_arg = t1, ft_res = t2 })
  * if (isPredTy t1 = True)  then af = InvisArg
  * if (isPredTy t1 = False) then af = VisArg
where isPredTy is defined in GHC.Core.Type, and sees if t1's
kind is Constraint.  See GHC.Core.TyCo.Rep
Note [Types for coercions, predicates, and evidence]

GHC.Core.Utils.mkFunctionType :: Mult -> Type -> Type -> Type
uses isPredTy to decide the AnonArgFlag for the FunTy.

The term (Lam b e), and coercion (FunCo co1 co2) don't carry
AnonArgFlags; instead they use mkFunctionType when we want to
get their types; see mkLamType and coercionLKind/RKind resp.
This is just an engineering choice; we could cache here too
if we wanted.

Why bother with all this? After all, we are in Core, where (=>) and
(->) behave the same.  We maintain this distinction throughout Core so
that we can cheaply and conveniently determine
* How to print a type
* How to split up a type: tcSplitSigmaTy
* How to specialise it (over type classes; GHC.Core.Opt.Specialise)

For the specialisation point, consider
(\ (d :: Ord a). blah).  We want to give it type
           (Ord a => blah_ty)
with a fat arrow; that is, using mkInvisFunTy, not mkVisFunTy.
Why?  Because the /specialiser/ treats dictionary arguments specially.
Suppose we do w/w on 'foo', thus (#11272, #6056)
   foo :: Ord a => Int -> blah
   foo a d x = case x of I# x' -> $wfoo @a d x'

   $wfoo :: Ord a => Int# -> blah

Now, at a call we see (foo @Int dOrdInt).  The specialiser will
specialise this to $sfoo, where
   $sfoo :: Int -> blah
   $sfoo x = case x of I# x' -> $wfoo @Int dOrdInt x'

Now we /must/ also specialise $wfoo!  But it wasn't user-written,
and has a type built with mkLamTypes.

Conclusion: the easiest thing is to make mkLamType build
            (c => ty)
when the argument is a predicate type.  See GHC.Core.TyCo.Rep
Note [Types for coercions, predicates, and evidence]
-}
