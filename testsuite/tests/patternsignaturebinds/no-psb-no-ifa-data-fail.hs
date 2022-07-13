{-# language NoImplicitForAll #-}
{-# language NoPatternSignatureBinds #-}

module ShouldFail where

import Data.Kind (Type)

data Psb2 (a :: c) :: forall b . b -> c -> Type
