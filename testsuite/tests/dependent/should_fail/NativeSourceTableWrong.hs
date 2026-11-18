{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}

module NativeSourceTableWrong where

import ClosedTableLibrary (flipBit)
import Data.Type.Equality ((:~:)(Refl))

type Computed = flipBit 'False

wrong :: Computed :~: 'False
wrong = Refl

data Flag (b :: Bool) = Flag

atBit :: foreach (b :: Bool) -> Flag b
atBit _b = Flag

wrongCall :: Flag False
wrongCall = atBit (flipBit False)

unknownCall :: (Bool -> Bool) -> Flag True
unknownCall f = atBit (f False)

class Step a where
    step :: a -> a

instance Step Bool where
    step = flipBit

localMethod :: Flag True
localMethod = atBit (step False)
