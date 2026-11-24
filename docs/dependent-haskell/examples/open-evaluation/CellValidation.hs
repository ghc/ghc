{-# LANGUAGE ExperimentalDependentTypes, NoUndecidableInstances, TypeFamilies #-}
-- | Keep empty cells empty and attach an error type to populated cells.
module CellValidation (Validate) where

-- | Choose the stored type for a cell.
type family CheckCell err cell where
  CheckCell err ()   = ()
  CheckCell err cell = Either err cell

-- | Supply the error type before the type of the cell is known.
type Validate err = \cell -> CheckCell err cell
