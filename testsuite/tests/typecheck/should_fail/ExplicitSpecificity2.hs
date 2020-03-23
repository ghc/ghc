{-# LANGUAGE TypeApplications, RankNTypes, GADTs, PolyKinds #-}

module ExplicitSpecificity2 where

import Data.Proxy
import Data.Kind

data T a where C :: forall {k} (a::k). Proxy a -> T a

bar :: ()
bar = let x = C @Type @Int Proxy
      in ()
