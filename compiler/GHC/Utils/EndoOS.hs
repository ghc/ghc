{-# LANGUAGE PatternSynonyms #-}

-- | One-shot endomorphisms
--   Mostly for backwards compatibility.

-- One-shot endomorphisms
-- Like GHC.Internal.Data.Semigroup.Internal.Endo, but uting
-- the one-shot trick from
--    Note [The one-shot state monad trick] in  GHC.Utils.Monad.

module GHC.Utils.EndoOS( EndoOS(EndoOS, appEndoOS ) ) where

import GHC.Prelude

import Data.Semigroup
import GHC.Exts (oneShot)

newtype EndoOS a = EndoOS' { appEndoOS :: a -> a }


instance Semigroup (EndoOS a) where
  f <> g = EndoOS (appEndoOS f . appEndoOS g)

instance Monoid (EndoOS a) where
   mempty  = EndoOS id

pattern EndoOS :: (a->a) -> EndoOS a
pattern EndoOS f <- EndoOS' f
      where
        EndoOS f = EndoOS' (oneShot f)
         -- oneShot: this is the core of the one-shot trick!
         -- Note [The one-shot state monad trick] in  GHC.Utils.Monad.
