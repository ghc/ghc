{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module T18252 where

import Data.Type.Equality
import GHC.TypeNats

eq :: (1 + 2 ~ 3) :~: ((1 + 2) ~ 3)
eq = Refl
