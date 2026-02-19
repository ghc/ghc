{-# LANGUAGE PatternSynonyms #-}

-- | One-shot endomorphisms
--   Mostly for backwards compatibility.

-- One-shot endomorphisms
-- Like GHC.Internal.Data.Semigroup.Internal.Endo, but using
-- the one-shot trick from
--    Note [The one-shot state monad trick] in  GHC.Utils.Monad.

module GHC.Utils.EndoOS( EndoOS(EndoOS, runEndoOS ), foldEndoOS ) where

import GHC.Prelude

import Data.Semigroup
import GHC.Exts (oneShot)

newtype EndoOS a = EndoOS' { runEndoOS :: a -> a }


instance Semigroup (EndoOS a) where
  f <> g = EndoOS (\x -> runEndoOS f $! (runEndoOS g x))
           -- Strict application, to avoid thunk creation

instance Monoid (EndoOS a) where
   mempty  = EndoOS id

pattern EndoOS :: (a->a) -> EndoOS a
pattern EndoOS f <- EndoOS' f
      where
        EndoOS f = EndoOS' (oneShot f)
         -- oneShot: this is the core of the one-shot trick!
         -- Note [The one-shot state monad trick] in  GHC.Utils.Monad.

foldEndoOS :: Foldable t => (a -> Endo b) -> t a -> Endo b
foldEndoOS f xs = foldr ((<>) . f) mempty xs
