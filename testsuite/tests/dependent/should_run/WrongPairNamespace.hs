{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module WrongPairNamespace where

import Data.Type.Equality ((:~:))
import qualified Prelude

wrong :: Prelude.fst '(Prelude.Int, Prelude.Bool) :~: Prelude.Int
wrong = Prelude.undefined
