{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Bug where

import Data.Kind
import Data.Proxy

foo :: forall (a :: Type)
              (f :: forall (x :: a). Proxy x -> Type).
       Proxy f -> ()
foo (_ :: _) = ()
