{-# Language RankNTypes       #-}
{-# Language PolyKinds        #-}
{-# Language TypeFamilies     #-}

module T15817 where

import Data.Kind

data family   X :: forall (a :: Type). Type
data instance X = MkX
