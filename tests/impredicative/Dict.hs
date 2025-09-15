-- This used to produce Core Lint errors

{-# Language ConstraintKinds       #-}
{-# Language DataKinds             #-}
{-# Language FlexibleInstances     #-}
{-# Language GADTs                 #-}
{-# Language ImpredicativeTypes    #-}
{-# Language KindSignatures        #-}
{-# Language MultiParamTypeClasses #-}
{-# Language PolyKinds             #-}
{-# Language QuantifiedConstraints #-}
{-# Language RankNTypes            #-}
{-# Language TypeOperators         #-}
{-# Language UndecidableInstances  #-}

{-# Options_GHC -dcore-lint #-}

module Dict where

import Data.Kind (Type)
import Data.Maybe (isJust, isNothing)
import Data.Type.Equality ((:~:))

data LoT k where
  LoT0   :: LoT Type
  (:&&:) :: k -> LoT ks -> LoT (k -> ks)

data Dict cls where
 Dict :: cls => Dict cls

class GTE a b x y (f :: LoT kf -> Type) (g :: LoT kg -> Type) where
  gTE :: f x -> g y -> Maybe (a :~: b)

class    (forall a b. GTE a b (a :&&: LoT0) (b :&&: LoT0) rep rep) => As f rep
instance (forall a b. GTE a b (a :&&: LoT0) (b :&&: LoT0) rep rep) => As f rep

ok :: Dict (As f rep) -> Dict (forall a b. GTE a b (a :&&: LoT0) (b :&&: LoT0) rep rep)
ok Dict = Dict
