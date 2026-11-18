{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}

module NativeSourceTableWrong where

import ClosedTableLibrary (flipBit)
import Data.Type.Equality ((:~:)(Refl))

type Computed = flipBit 'False

wrong :: Computed :~: 'False
wrong = Refl
