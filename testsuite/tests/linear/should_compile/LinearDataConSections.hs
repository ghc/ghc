{-# LANGUAGE DataKinds, LinearTypes, GADTSyntax #-}

module LinearDataConSections where

import GHC.Types ( Multiplicity(..) )

-- Check that we correctly eta-expand left and right sections
-- of data-constructors to change multiplicities from One to Many

data D where
  MkD :: Bool %1 -> Char %1 -> D

foo :: Char %Many -> D
foo = (True `MkD`)

bar :: Bool %Many -> D
bar = (`MkD` 'y')