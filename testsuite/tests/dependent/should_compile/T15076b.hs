{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
module Bug where

import Data.Kind
import Data.Proxy

foo :: forall (a :: Type)
              (f :: forall (x :: a). Proxy x -> Type).
       Proxy f -> ()
foo _ = ()
