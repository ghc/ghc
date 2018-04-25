{-# LANGUAGE FlexibleContexts #-}
module Distribution.Compat.Parsec (
    P.Parsec,
    P.ParsecT,
    P.Stream,
    (P.<?>),

    P.runParser,

    -- * Combinators
    P.between,
    P.option,
    P.optional,
    P.optionMaybe,
    P.try,
    P.sepBy,
    P.sepBy1,
    P.choice,
    P.eof,

    -- * Char
    integral,
    P.char,
    P.anyChar,
    P.satisfy,
    P.space,
    P.spaces,
    skipSpaces1,
    P.string,
    munch,
    munch1,
    P.oneOf,
    ) where

import           Distribution.Compat.Prelude
import           Prelude ()

import qualified Text.Parsec                 as P
import qualified Text.Parsec.Pos             as P

integral :: (P.Stream s m Char, Integral a) => P.ParsecT s u m a
integral = toNumber <$> some d P.<?> "integral"
  where
    toNumber = foldl' (\a b -> a * 10 + b) 0
    d = P.tokenPrim
          (\c -> show [c])
          (\pos c _cs -> P.updatePosChar pos c)
          f
    f '0' = Just 0
    f '1' = Just 1
    f '2' = Just 2
    f '3' = Just 3
    f '4' = Just 4
    f '5' = Just 5
    f '6' = Just 6
    f '7' = Just 7
    f '8' = Just 8
    f '9' = Just 9
    f _   = Nothing

-- | Greedily munch characters while predicate holds.
-- Require at least one character.
munch1
    :: P.Stream s m Char
    => (Char -> Bool)
    -> P.ParsecT s u m String
munch1 = some . P.satisfy

-- | Greedely munch characters while predicate holds.
-- Always succeeds.
munch
    :: P.Stream s m Char
    => (Char -> Bool)
    -> P.ParsecT s u m String
munch = many . P.satisfy

skipSpaces1 :: P.Stream s m Char => P.ParsecT s u m ()
skipSpaces1 = P.skipMany1 P.space
