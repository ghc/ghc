{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module T18252a where

import Data.Type.Equality
import GHC.TypeNats

eq :: (a ~ b ~ c) :~: ()
eq = Refl
