{-# LANGUAGE DeepSubsumption #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeAbstractions #-}

module T26331 where

import Data.Kind (Constraint, Type)

type Apply :: (k1 ~> k2) -> k1 -> k2
type family Apply (f :: k1 ~> k2) (x :: k1) :: k2

type (~>) :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>

data TyFun :: Type -> Type -> Type

type Sing :: k -> Type
type family Sing @k :: k -> Type

type SingFunction2 :: (a1 ~> a2 ~> b) -> Type
type SingFunction2 (f :: a1 ~> a2 ~> b) =
  forall t1 t2. Sing t1 -> Sing t2 -> Sing (f `Apply` t1 `Apply` t2)

unSingFun2 :: forall f. Sing f -> SingFunction2 f
-- unSingFun2 :: forall f. Sing f -> forall t1 t2. blah
unSingFun2 sf x = error "urk"

singFun2 :: forall f. SingFunction2 f -> Sing f
singFun2 f = error "urk"

-------- This is the tricky bit -------
pattern SLambda2 :: forall f. SingFunction2 f -> Sing f
pattern SLambda2 x <- (unSingFun2 -> x)    -- We want to push down (SingFunction2 f)
                                           -- /uninstantiated/ into the pattern `x`
  where
    SLambda2 lam2         = singFun2 lam2

