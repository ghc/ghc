{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Bug where

import Data.Kind
import Data.Proxy

foo :: forall (a :: Type)
              (f :: forall (x :: a). Proxy x -> Type).
       Proxy f -> ()
foo _ = ()
