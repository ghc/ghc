module Distribution.Solver.Types.Progress
    ( Progress(..)
    , foldProgress
    ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude hiding (fail)

-- | A type to represent the unfolding of an expensive long running
-- calculation that may fail. We may get intermediate steps before the final
-- result which may be used to indicate progress and\/or logging messages.
--
data Progress step fail done = Step step (Progress step fail done)
                             | Fail fail
                             | Done done

-- This Functor instance works around a bug in GHC 7.6.3.
-- See https://ghc.haskell.org/trac/ghc/ticket/7436#comment:6.
-- The derived functor instance caused a space leak in the solver.
instance Functor (Progress step fail) where
  fmap f (Step s p) = Step s (fmap f p)
  fmap _ (Fail x)   = Fail x
  fmap f (Done r)   = Done (f r)

-- | Consume a 'Progress' calculation. Much like 'foldr' for lists but with two
-- base cases, one for a final result and one for failure.
--
-- Eg to convert into a simple 'Either' result use:
--
-- > foldProgress (flip const) Left Right
--
foldProgress :: (step -> a -> a) -> (fail -> a) -> (done -> a)
             -> Progress step fail done -> a
foldProgress step fail done = fold
  where fold (Step s p) = step s (fold p)
        fold (Fail f)   = fail f
        fold (Done r)   = done r

instance Monad (Progress step fail) where
  return   = pure
  p >>= f  = foldProgress Step Fail f p

instance Applicative (Progress step fail) where
  pure a  = Done a
  p <*> x = foldProgress Step Fail (flip fmap x) p

instance Monoid fail => Alternative (Progress step fail) where
  empty   = Fail mempty
  p <|> q = foldProgress Step (const q) Done p
