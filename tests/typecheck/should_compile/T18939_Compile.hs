{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module T18939_Compile where

import Data.Kind

data family   Hm :: forall a -> a -> Type
data instance Hm :: forall a -> a -> Type
