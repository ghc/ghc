{-# LANGUAGE PatternSynonyms #-}

-- | One-shot endomorphisms

-- One-shot endomorphisms
-- Like GHC.Internal.Data.Semigroup.Internal.Endo, but using
-- the one-shot trick from
--    Note [The one-shot state monad trick] in  GHC.Utils.Monad.
--
-- It is also strict: see the (<>) method in he Semigroup instance

module GHC.Utils.EndoOS( EndoOS(EndoOS, runEndoOS ), foldEndoOS ) where

import GHC.Prelude

import Data.Semigroup
import GHC.Exts (oneShot)

newtype EndoOS a = EndoOS' { runEndoOS :: a -> a }


instance Semigroup (EndoOS a) where
  EndoOS' f <> EndoOS' g = EndoOS (\x -> g $! f x)
     -- NB1: Strict application, to avoid thunk creation
     --      See (FV3) in Note [Finding free variables]
     --      in GHC.Types.Var.FV
     -- NB2: We apply `f` to the acccumulator first, then `g`
     --      So if we traverse a type left-to-right, the insertion
     --      order for (say) free type variables is left-to-right
     --      See (FV4) in Note [Finding free variables]
     --      in GHC.Types.Var.FV

instance Monoid (EndoOS a) where
   mempty  = EndoOS id

pattern EndoOS :: (a->a) -> EndoOS a
{-# COMPLETE EndoOS #-}
pattern EndoOS f <- EndoOS' f
      where
        EndoOS f = EndoOS' (oneShot f)
         -- oneShot: this is the core of the one-shot trick!
         -- Note [The one-shot state monad trick] in  GHC.Utils.Monad.

foldEndoOS :: Foldable t => (a -> Endo b) -> t a -> Endo b
foldEndoOS f xs = foldr ((<>) . f) mempty xs
