{-# LANGUAGE AllowAmbiguousTypes, GADTs, Modifiers, PolyKinds, RankNTypes, TypeOperators #-}

-- | List length laws and their equality witness.
module HaskLib
    ( module HaskLib.List
    , plusZero, plusSucc, congSucc, congFirst
    , (:~:) (Refl), cong, trans, withEquality
    ) where

import Data.Type.Equality ((:~:) (Refl), gcastWith, trans)
import GHC.Matchability (Matchability)
import HaskLib.List
import HaskLib.Size (congFirst, congSucc, plusSucc, plusZero)

-- | Apply the same type constructor or type function to both endpoints.
cong :: forall {a} {b} {m :: Matchability} (f :: a %m -> b) x y.
        x :~: y -> f x :~: f y
cong Refl = Refl

-- | Use an equality witness to bring @a ~ b@ into scope for the second argument.
withEquality :: (a :~: b) -> ((a ~ b) => r) -> r
withEquality = gcastWith
