-- |
-- Module      :  Haddock.Parser
-- Copyright   :  (c) Mateusz Kowalczyk 2013,
--                    Simon Hengel      2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
module Haddock.Parser
  ( parseParas
  , parseString
  , parseIdent
  ) where

import GHC.Data.FastString (fsLit)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Parser (parseIdentifier)
import GHC.Parser.Lexer (ParseResult (PFailed, POk), ParserOpts, initParserState, unP)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SrcLoc (GenLocated (..), mkRealSrcLoc)

import qualified Documentation.Haddock.Parser as P
import Documentation.Haddock.Types
import Haddock.Types

parseParas :: ParserOpts -> Maybe Package -> String -> MetaDoc mod (Wrap NsRdrName)
parseParas parserOpts p = overDoc (P.overIdentifier (parseIdent parserOpts)) . P.parseParas p

parseString :: ParserOpts -> String -> DocH mod (Wrap NsRdrName)
parseString parserOpts = P.overIdentifier (parseIdent parserOpts) . P.parseString

parseIdent :: ParserOpts -> Namespace -> String -> Maybe (Wrap NsRdrName)
parseIdent parserOpts ns str0 =
  case unP parseIdentifier (pstate str1) of
    POk _ (L _ name)
      -- Guards against things like 'Q.--', 'Q.case', etc.
      -- See https://github.com/haskell/haddock/issues/952 and Trac #14109
      | Qual _ occ <- name
      , PFailed{} <- unP parseIdentifier (pstate (occNameString occ)) ->
          Nothing
      | otherwise ->
          Just (wrap (NsRdrName ns name))
    PFailed{} -> Nothing
  where
    realSrcLc = mkRealSrcLoc (fsLit "<unknown file>") 0 0
    pstate str = initParserState parserOpts (stringToStringBuffer str) realSrcLc
    (wrap, str1) = case str0 of
      '(' : s@(c : _)
        | c /= ','
        , c /= ')' -> -- rule out tuple names
            (Parenthesized, init s)
      '`' : s@(_ : _) -> (Backticked, init s)
      _ -> (Unadorned, str0)
