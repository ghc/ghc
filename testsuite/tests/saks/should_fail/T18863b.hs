{-# Language PolyKinds                #-}
{-# Language RankNTypes               #-}
{-# Language StandaloneKindSignatures #-}
module T18863b where

import Data.Kind

type IDb :: forall i.   i -> Type
data IDb :: forall i -> i -> Type
