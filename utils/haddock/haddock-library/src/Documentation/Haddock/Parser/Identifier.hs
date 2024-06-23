{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Documentation.Haddock.Parser.Identifier
-- Copyright   :  (c) Alec Theriault 2019,
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Functionality for parsing identifiers and operators
module Documentation.Haddock.Parser.Identifier
  ( Identifier (..)
  , parseValid
  ) where

import Control.Monad (guard)
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor (($>))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
  ( State (..)
  , getParserState
  , setParserState
  )
import qualified Text.Parsec as Parsec
import Text.Parsec.Pos (updatePosChar)
import Text.Read.Lex (isSymbolChar)

import Documentation.Haddock.Parser.Monad
import Documentation.Haddock.Types (Namespace (..))

-- | Identifier string surrounded with namespace, opening, and closing quotes/backticks.
data Identifier = Identifier !Namespace !Char String !Char
  deriving (Show, Eq)

parseValid :: Parser Identifier
parseValid = do
  s@State{stateInput = inp, statePos = pos} <- getParserState

  case takeIdentifier inp of
    Nothing -> Parsec.parserFail "parseValid: Failed to match a valid identifier"
    Just (ns, op, ident, cl, inp') ->
      let posOp = updatePosChar pos op
          posIdent = T.foldl updatePosChar posOp ident
          posCl = updatePosChar posIdent cl
          s' = s{stateInput = inp', statePos = posCl}
       in setParserState s' $> Identifier ns op (T.unpack ident) cl

-- | Try to parse a delimited identifier off the front of the given input.
--
-- This tries to match as many valid Haskell identifiers/operators as possible,
-- to the point of sometimes accepting invalid things (ex: keywords). Some
-- considerations:
--
--   - operators and identifiers can have module qualifications
--   - operators can be wrapped in parens (for prefix)
--   - identifiers can be wrapped in backticks (for infix)
--   - delimiters are backticks or regular ticks
--   - since regular ticks are also valid in identifiers, we opt for the
--     longest successful parse
--
-- This function should make /O(1)/ allocations
takeIdentifier :: Text -> Maybe (Namespace, Char, Text, Char, Text)
takeIdentifier input = listToMaybe $ do
  -- Optional namespace
  let (ns, input') = case T.uncons input of
        Just ('v', i) -> (Value, i)
        Just ('t', i) -> (Type, i)
        _ -> (None, input)

  -- Opening tick
  (op, input'') <- maybeToList (T.uncons input')
  guard (op == '\'' || op == '`')

  -- Identifier/operator
  (ident, input''') <- wrapped input''

  -- Closing tick
  (cl, input'''') <- maybeToList (T.uncons input''')
  guard (cl == '\'' || cl == '`')

  return (ns, op, ident, cl, input'''')
  where
    -- \| Parse out a wrapped, possibly qualified, operator or identifier
    wrapped t = do
      (c, t') <- maybeToList (T.uncons t)
      -- Tuples
      case c of
        '('
          | Just (c', _) <- T.uncons t'
          , c' == ',' || c' == ')' ->
              do
                let (commas, t'') = T.span (== ',') t'
                (')', t''') <- maybeToList (T.uncons t'')
                return (T.take (T.length commas + 2) t, t''')

        -- Parenthesized
        '(' -> do
          (n, t'') <- general False 0 [] t'
          (')', t''') <- maybeToList (T.uncons t'')
          return (T.take (n + 2) t, t''')

        -- Backticked
        '`' -> do
          (n, t'') <- general False 0 [] t'
          ('`', t''') <- maybeToList (T.uncons t'')
          return (T.take (n + 2) t, t''')

        -- Unadorned
        _ -> do
          (n, t'') <- general False 0 [] t
          return (T.take n t, t'')

    -- \| Parse out a possibly qualified operator or identifier
    general
      :: Bool
      -- \^ refuse inputs starting with operators
      -> Int
      -- \^ total characters \"consumed\" so far
      -> [(Int, Text)]
      -- \^ accumulated results
      -> Text
      -- \^ current input
      -> [(Int, Text)]
    -- \^ total characters parsed & what remains
    general !identOnly !i acc t
      -- Starts with an identifier (either just an identifier, or a module qual)
      | Just (n, rest) <- identLike t =
          if T.null rest
            then acc
            else case T.head rest of
              '`' -> (n + i, rest) : acc
              ')' -> (n + i, rest) : acc
              '.' -> general False (n + i + 1) acc (T.tail rest)
              '\'' ->
                let (m, rest') = quotes rest
                 in general True (n + m + 1 + i) ((n + m + i, rest') : acc) (T.tail rest')
              _ -> acc
      -- An operator
      | Just (n, rest) <- optr t
      , not identOnly =
          (n + i, rest) : acc
      -- Anything else
      | otherwise =
          acc

    -- \| Parse an identifier off the front of the input
    identLike t
      | T.null t = Nothing
      | isAlpha (T.head t) || '_' == T.head t =
          let !(idt, rest) = T.span (\c -> isAlphaNum c || c == '_') t
              !(octos, rest') = T.span (== '#') rest
           in Just (T.length idt + T.length octos, rest')
      | otherwise = Nothing

    -- \| Parse all but the last quote off the front of the input
    -- PRECONDITION: T.head t `elem` ['\'', '`']
    quotes :: Text -> (Int, Text)
    quotes t =
      let !n = T.length (T.takeWhile (`elem` ['\'', '`']) t) - 1
       in (n, T.drop n t)

    -- \| Parse an operator off the front of the input
    optr t =
      let !(op, rest) = T.span isSymbolChar t
       in if T.null op then Nothing else Just (T.length op, rest)
