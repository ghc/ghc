{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Enum (Enum) where

-- For why this file exists
-- See Note [Semigroup stimes cycle] in GHC.Internal.Base

import GHC.Types ()

class Enum a
