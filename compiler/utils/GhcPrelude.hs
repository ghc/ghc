-- | Custom GHC "Prelude"
--
-- This module serves as a replacement for the "Prelude" module
-- and abstracts over differences between the bootstrapping
-- GHC version, and may also provide a common default vocabulary.
--
module GhcPrelude (module X) where

import Prelude as X
