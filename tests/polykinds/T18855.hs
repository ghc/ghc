{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Bug where

import Data.Kind

type family Apply (f :: a -> b) (x :: a) :: b

type F :: forall a.
          forall (p :: forall bOne. Either a bOne -> Type)
       -> forall bTwo.
          forall (e :: Either a bTwo)
       -> Apply p e

type family F
