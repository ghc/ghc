{-# LANGUAGE DefaultSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Text
-- Copyright   :  Duncan Coutts 2007
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defines a 'Text' class which is a bit like the 'Read' and 'Show'
-- classes. The difference is that is uses a modern pretty printer and parser
-- system and the format is not expected to be Haskell concrete syntax but
-- rather the external human readable representation used by Cabal.
--
module Distribution.Text (
  Text(..),
  defaultStyle,
  display,
  flatStyle,
  simpleParse,
  stdParse,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import           Data.Functor.Identity    (Identity (..))
import           Distribution.Pretty
import           Distribution.Parsec.Class
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp

import Data.Version (Version(Version))

-- | /Note:/ this class will soon be deprecated.
-- It's not yet, so we are @-Wall@ clean.
class Text a where
  disp  :: a -> Disp.Doc
  default disp :: Pretty a => a -> Disp.Doc
  disp = pretty

  parse :: Parse.ReadP r a
  default parse :: Parsec a => Parse.ReadP r a
  parse = Parse.parsecToReadP parsec []

-- | Pretty-prints with the default style.
display :: Text a => a -> String
display = Disp.renderStyle defaultStyle . disp

simpleParse :: Text a => String -> Maybe a
simpleParse str = case [ p | (p, s) <- Parse.readP_to_S parse str
                       , all isSpace s ] of
  []    -> Nothing
  (p:_) -> Just p

stdParse :: Text ver => (ver -> String -> res) -> Parse.ReadP r res
stdParse f = do
  cs   <- Parse.sepBy1 component (Parse.char '-')
  _    <- Parse.char '-'
  ver  <- parse
  let name = intercalate "-" cs
  return $! f ver (lowercase name)
  where
    component = do
      cs <- Parse.munch1 isAlphaNum
      if all isDigit cs then Parse.pfail else return cs
      -- each component must contain an alphabetic character, to avoid
      -- ambiguity in identifiers like foo-1 (the 1 is the version number).

lowercase :: String -> String
lowercase = map toLower

-- -----------------------------------------------------------------------------
-- Instances for types from the base package

instance Text Bool where
  parse = Parse.choice [ (Parse.string "True" Parse.+++
                          Parse.string "true") >> return True
                       , (Parse.string "False" Parse.+++
                          Parse.string "false") >> return False ]

instance Text Int where
  parse = fmap negate (Parse.char '-' >> parseNat) Parse.+++ parseNat

instance Text a => Text (Identity a) where
    disp = disp . runIdentity
    parse = fmap Identity parse

-- | Parser for non-negative integers.
parseNat :: Parse.ReadP r Int
parseNat = read `fmap` Parse.munch1 isDigit -- TODO: eradicateNoParse


instance Text Version where
  disp (Version branch _tags)     -- Death to version tags!!
    = Disp.hcat (Disp.punctuate (Disp.char '.') (map Disp.int branch))

  parse = do
      branch <- Parse.sepBy1 parseNat (Parse.char '.')
                -- allow but ignore tags:
      _tags  <- Parse.many (Parse.char '-' >> Parse.munch1 isAlphaNum)
      return (Version branch [])
