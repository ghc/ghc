{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Enum (Enum) where

-- For why this file exists
-- See Note [Semigroup stimes cycle] in GHC.Base

import GHC.Types ()

class Enum a
