{-# LANGUAGE TypeFamilies #-}

-- | Configuration options for Lift the lambda lifter.
module GHC.Stg.Lift.Config (
    StgLiftConfig (..),
  ) where

import GHC.Prelude

import GHC.Platform.Profile

data StgLiftConfig = StgLiftConfig
  { c_targetProfile         :: !Profile
  , c_liftLamsRecArgs       :: !(Maybe Int)
  -- ^ Maximum number of arguments after lambda lifting a recursive function.
  , c_liftLamsNonRecArgs    :: !(Maybe Int)
  -- ^ Maximum number of arguments after lambda lifting non-recursive function.
  , c_liftLamsKnown         :: !Bool
  -- ^ Lambda lift even when this turns a known call into an unknown call.
  }
  deriving (Show, Read, Eq, Ord)
