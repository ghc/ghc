{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T15568 where

import Data.Proxy

type family F (a :: j) :: k
