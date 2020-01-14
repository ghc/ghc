{-# LANGUAGE TypeApplications, RankNTypes, GADTs, PolyKinds #-}

module ExplicitSpecificity3 where

import Data.Proxy
import Data.Kind

data T (a :: k) = C { f2 :: Proxy a }

pattern Pat {k} a = C @k @a Proxy

bar :: (T a) -> ()
bar (Pat @Type @Int) = ()
