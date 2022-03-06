{-# LANGUAGE TypeFamilies #-}
module T21185 where

import Data.Kind (Type)
import GHC.Generics (Generic1(..))

type FakeOut x = Int

data D (b :: Type) where
  MkD :: c -> FakeOut c -> D c
  deriving Generic1

data family DF (a :: Type)
data instance DF (b :: Type) where
  MkDF :: c -> FakeOut c -> DF c
  deriving Generic1

d :: Rep1 D ()
d = from1 $ MkD () 42

df :: Rep1 DF ()
df = from1 $ MkDF () 42
