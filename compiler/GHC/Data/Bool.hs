module GHC.Data.Bool
  ( OverridingBool(..)
  , overrideWith
  )
where

import GHC.Prelude

data OverridingBool
  = Auto
  | Never
  | Always
  deriving
    ( Show
    , Read    -- ^ @since 9.4.1
    , Eq      -- ^ @since 9.4.1
    , Ord     -- ^ @since 9.4.1
    , Enum    -- ^ @since 9.4.1
    , Bounded -- ^ @since 9.4.1
    )

overrideWith :: Bool -> OverridingBool -> Bool
overrideWith b Auto   = b
overrideWith _ Never  = False
overrideWith _ Always = True
