module GHC.Data.Bool
  ( OverridingBool(..)
  , overrideWith
  )
where

import GHC.Prelude

data OverridingBool
  = Auto
  | Always
  | Never
  deriving Show

overrideWith :: Bool -> OverridingBool -> Bool
overrideWith b Auto   = b
overrideWith _ Always = True
overrideWith _ Never  = False
