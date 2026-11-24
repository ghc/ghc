{-# LANGUAGE GADTs, PolyKinds, RankNTypes, TypeOperators #-}

-- | List length laws and their equality witness.
module HaskLib
    ( module HaskLib.List
    , plusZero, plusSucc, congSucc, congFirst
    , (:~:) (Refl), trans, rewrite, withEquality
    ) where

import Data.Type.Equality ((:~:) (Refl), gcastWith, trans)
import HaskLib.List
import HaskLib.Size (congFirst, congSucc, plusSucc, plusZero)

-- | Use an equality witness to bring @a ~ b@ into scope for the second argument.
rewrite :: (a :~: b) -> ((a ~ b) => r) -> r
rewrite = gcastWith

-- | Compatibility name for 'rewrite'.
withEquality :: (a :~: b) -> ((a ~ b) => r) -> r
withEquality = rewrite
