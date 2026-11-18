{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module NativeClosedCharWrong where

import qualified Data.Char
import Data.Type.Equality ((:~:)(Refl))

wrong :: Data.Char.isDigit '7' :~: False
wrong = Refl
