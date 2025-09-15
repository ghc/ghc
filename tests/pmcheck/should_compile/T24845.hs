{-# LANGUAGE DataKinds, GADTs #-}
{-# OPTIONS_GHC -Wall #-}
module M where

import Data.Proxy
import GHC.TypeLits

data T n where
  Z :: T 0
  S :: Proxy n -> T (n + 1)

foo :: T 0 -> Bool
foo Z = True
-- Should not warn of non-exhaustive patterns
