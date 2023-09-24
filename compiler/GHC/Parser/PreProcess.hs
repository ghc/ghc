-- Implement a subset of CPP, sufficient for conditional compilation
-- (only)

-- Note: this file formatted with fourmolu

module GHC.Parser.PreProcess (
    lexer,
    lexerDbg,
) where

import GHC.Data.FastString
import GHC.Data.Maybe
import GHC.Data.OrdList
import GHC.Data.StringBuffer
import GHC.Types.Error
import GHC.Types.Unique.FM
import GHC.Utils.Error
import GHC.Utils.Misc (readHexSignificandExponentPair, readSignificandExponentPair)
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Hs.Doc
import GHC.Types.Basic (InlineSpec (..), RuleMatchInfo (..))
import GHC.Types.SourceText
import GHC.Types.SrcLoc

import GHC.Parser.CharClass

import Debug.Trace (trace)
import GHC.Driver.Flags
import GHC.Parser.Annotation
import GHC.Parser.Errors.Basic
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Errors.Types
import GHC.Parser.Lexer (P (..), Token (..))
import qualified GHC.Parser.Lexer as Lexer
import GHC.Prelude

-- ---------------------------------------------------------------------

lexer, lexerDbg :: Bool -> (Located Token -> P a) -> P a
lexer queueComments cont = do
    Lexer.lexer queueComments cont

-- Use this instead of 'lexer' in GHC.Parser to dump the tokens for debugging.
lexerDbg queueComments cont = lexer queueComments contDbg
  where
    contDbg tok = trace ("ptoken: " ++ show (unLoc tok)) (cont tok)
