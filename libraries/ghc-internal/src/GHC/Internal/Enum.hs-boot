{-# LANGUAGE NoImplicitPrelude #-}

-- For why this file exists
-- See Note [Semigroup stimes cycle] in GHC.Internal.Base

module GHC.Internal.Enum (Enum) where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Types ()

class Enum a
