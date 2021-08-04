-- | This module contains an arrow interface for option parsers, which allows
-- to define and combine parsers using the arrow notation and arrow
-- combinators.
--
-- The arrow syntax is particularly useful to create parsers of nested
-- structures, or records where the order of fields is different from the order
-- in which the parsers should be applied.
--
-- For example, an 'Options.Applicative.Builder.arguments` parser often needs
-- to be applied last, and that makes it inconvenient to use it for a field
-- which is not the last one in a record.
--
-- Using the arrow syntax and the functions in this module, one can write, e.g.:
--
-- > data Options = Options
-- >   { optArgs :: [String]
-- >   , optVerbose :: Bool }
-- >
-- > opts :: Parser Options
-- > opts = runA $ proc () -> do
-- >   verbose <- asA (switch (short 'v')) -< ()
-- >   args <- asA (arguments str idm) -< ()
-- >   returnA -< Options args verbose
--
-- Parser arrows, created out of regular 'Parser' values using the 'asA'
-- function, are arrows taking @()@ as argument and returning the parsed value.
module Options.Applicative.Arrows (
  module Control.Arrow,
  A(..),
  asA,
  runA,
  ParserA,
  ) where

import Control.Arrow
import Control.Category (Category(..))

import Options.Applicative

import Prelude hiding ((.), id)

-- | For any 'Applicative' functor @f@, @A f@ is the 'Arrow' instance
-- associated to @f@.
--
-- The 'A' constructor can be used to convert a value of type @f (a -> b)@ into
-- an arrow.
newtype A f a b = A
  { unA :: f (a -> b) }

-- | Convert a value of type @f a@ into an arrow taking @()@ as argument.
--
-- Applied to a value of type 'Parser', it turns it into an arrow that can be
-- used inside an arrow command, or passed to arrow combinators.
asA :: Applicative f => f a -> A f () a
asA x = A $ const <$> x

-- | Convert an arrow back to an applicative value.
--
-- This function can be used to return a result of type 'Parser' from an arrow
-- command.
runA :: Applicative f => A f () a -> f a
runA a = unA a <*> pure ()

instance Applicative f => Category (A f) where
  id = A $ pure id
  -- use reverse composition, because we want effects to run from
  -- top to bottom in the arrow syntax
  (A f) . (A g) = A $ flip (.) <$> g <*> f

instance Applicative f => Arrow (A f) where
  arr = A . pure
  first (A f) = A $ first <$> f

-- | The type of arrows associated to the applicative 'Parser' functor.
type ParserA = A Parser
