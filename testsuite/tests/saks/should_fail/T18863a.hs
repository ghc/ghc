{-# Language PolyKinds                #-}
{-# Language RankNTypes               #-}
{-# Language StandaloneKindSignatures #-}
module T18863a where

import Data.Kind

type IDa :: forall i -> i -> Type
data IDa :: forall i.   i -> Type
