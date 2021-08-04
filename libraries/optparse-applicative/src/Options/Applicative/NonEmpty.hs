{-# LANGUAGE CPP, Rank2Types, ExistentialQuantification #-}
module Options.Applicative.NonEmpty (
  some1
) where

import Data.List.NonEmpty (NonEmpty (..))

import Options.Applicative.Types
import Control.Applicative
import Prelude

-- | Sequences an action one or more times.
--
--   Functionally identical to 'Data.List.NonEmpty.some1',
--   but is preferred as it gives a nicer help text.
some1 :: Parser a -> Parser (NonEmpty a)
some1 p = fromM $ (:|) <$> oneM p <*> manyM p
