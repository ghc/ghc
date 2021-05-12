{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, TypeFamilies #-}

module T18862 where

import Prelude (Bool)
import Data.Kind (Constraint)
import qualified Data.Type.Equality as E

type family (a :: k) ~ (b :: k) :: result_kind

type instance a ~ b = (a E.~ b :: Constraint)
type instance a ~ b = (a E.== b :: Bool)
