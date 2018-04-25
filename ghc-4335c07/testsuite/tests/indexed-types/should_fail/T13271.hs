{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module T13271 where

import GHC.TypeLits

data T1 = T1
type T2 = TypeError (Text "You can't do that!")

type family X i = r | r -> i where
  X 1 = T1
  X 2 = T2
