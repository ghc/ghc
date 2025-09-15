{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module T25834 where

import Data.Kind (Type)
import Language.Haskell.TH hiding (Type)

-- We recognize different inputs from the user
data Inputs = A | B
data AInput = AInput Int
data BInput = BInput Char
data Interface = MyInterface

type family InputData (i :: Inputs) where
  InputData 'A = AInput
  InputData 'B = BInput

type family MyInput (w :: k) :: (r :: Type) | r -> w
type family MyInputData (w :: k) (i :: MyInput w)

$( [d| foo x = x + 1 |]  )

type instance MyInput 'MyInterface = Inputs
type instance MyInputData 'MyInterface a = InputData a

