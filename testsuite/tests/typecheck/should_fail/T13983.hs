{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T13983 where

import Data.Proxy

type Wat = forall (a :: k). Proxy a
