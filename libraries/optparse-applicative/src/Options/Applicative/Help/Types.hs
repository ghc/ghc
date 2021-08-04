module Options.Applicative.Help.Types (
    ParserHelp (..)
  , renderHelp
  ) where

import Data.Semigroup
import Prelude

import Options.Applicative.Help.Chunk
import Options.Applicative.Help.Pretty

data ParserHelp = ParserHelp
  { helpError :: Chunk Doc
  , helpSuggestions :: Chunk Doc
  , helpHeader :: Chunk Doc
  , helpUsage :: Chunk Doc
  , helpBody :: Chunk Doc
  , helpGlobals :: Chunk Doc
  , helpFooter :: Chunk Doc }

instance Show ParserHelp where
  showsPrec _ h = showString (renderHelp 80 h)

instance Monoid ParserHelp where
  mempty = ParserHelp mempty mempty mempty mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup ParserHelp where
  (ParserHelp e1 s1 h1 u1 b1 g1 f1) <> (ParserHelp e2 s2 h2 u2 b2 g2 f2)
    = ParserHelp (mappend e1 e2) (mappend s1 s2)
                 (mappend h1 h2) (mappend u1 u2)
                 (mappend b1 b2) (mappend g1 g2)
                 (mappend f1 f2)

helpText :: ParserHelp -> Doc
helpText (ParserHelp e s h u b g f) = extractChunk . vsepChunks $ [e, s, h, u, b, g, f]

-- | Convert a help text to 'String'.
renderHelp :: Int -> ParserHelp -> String
renderHelp cols
  = (`displayS` "")
  . renderPretty 1.0 cols
  . helpText
