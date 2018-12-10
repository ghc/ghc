{-# Language PolyKinds          #-}
{-# Language TypeApplications   #-}
{-# Language ImpredicativeTypes #-}
{-# Language LiberalTypeSynonyms #-}

module T15859 where

import Data.Kind

data A k :: k -> Type

type KindOf (a :: k) = k

a = (undefined :: KindOf A) @Int
