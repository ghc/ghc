{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module T13738 where

import Data.Coerce
import Data.Proxy

foo x = coerce @(forall (a :: k). Proxy a -> Int)
               @(forall (a :: k). Proxy a -> Int)
        x
