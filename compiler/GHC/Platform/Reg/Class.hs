{-# LANGUAGE LambdaCase #-}
-- | An architecture independent description of a register's class.
module GHC.Platform.Reg.Class
        ( RegClass (..)
        , allRegClasses
        )

where

import GHC.Prelude

import GHC.Utils.Outputable as Outputable
import GHC.Types.Unique
import GHC.Builtin.Uniques ( mkRegClassUnique )


-- | The class of a register.
--      Used in the register allocator.
--      We treat all registers in a class as being interchangeable.
--
data RegClass
  -- | Supports (scalar) integers only.
  = RcInteger
  -- | Supports vectors (both integers & floats) as well as scalar values
  -- (but in practice not used for scalar integer values).
  | RcFloatOrVector
  deriving (Eq, Ord, Show)

allRegClasses :: [RegClass]
allRegClasses = [RcInteger, RcFloatOrVector]

instance Uniquable RegClass where
    getUnique = \case
      RcInteger       -> mkRegClassUnique 0
      RcFloatOrVector -> mkRegClassUnique 1

instance Outputable RegClass where
    ppr = \case
      RcInteger       -> Outputable.text "I"
      RcFloatOrVector -> Outputable.text "F"
