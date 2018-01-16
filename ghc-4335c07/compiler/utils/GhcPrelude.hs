{-# LANGUAGE CPP #-}

-- | Custom GHC "Prelude"
--
-- This module serves as a replacement for the "Prelude" module
-- and abstracts over differences between the bootstrapping
-- GHC version, and may also provide a common default vocabulary.
--
module GhcPrelude (module X) where

-- We export the 'Semigroup' class but w/o the (<>) operator to avoid
-- clashing with the (Outputable.<>) operator which is heavily used
-- through GHC's code-base.

#if MIN_VERSION_base(4,11,0)
import Prelude as X hiding ((<>))
#else
import Prelude as X
import Data.Semigroup as X (Semigroup)
#endif
