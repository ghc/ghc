-- |
-- Module      :  Haddock.Parser
-- Copyright   :  (c) Mateusz Kowalczyk 2013,
--                    Simon Hengel      2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable

module Haddock.Parser ( parseParas
                      , parseString
                      , parseIdent
                      ) where

import qualified Documentation.Haddock.Parser as P
import Documentation.Haddock.Types
import Haddock.Types (NsRdrName(..))

import DynFlags     ( DynFlags )
import FastString   ( fsLit )
import Lexer        ( mkPState, unP, ParseResult(POk) )
import Parser       ( parseIdentifier )
import RdrName      ( RdrName )
import SrcLoc       ( mkRealSrcLoc, GenLocated(..) )
import StringBuffer ( stringToStringBuffer )

parseParas :: DynFlags -> Maybe Package -> String -> MetaDoc mod NsRdrName
parseParas d p = overDoc (P.overIdentifier (parseIdent d)) . P.parseParas p

parseString :: DynFlags -> String -> DocH mod NsRdrName
parseString d = P.overIdentifier (parseIdent d) . P.parseString

parseIdent :: DynFlags -> Namespace -> String -> Maybe NsRdrName
parseIdent dflags ns str0 =
  let buffer = stringToStringBuffer str0
      realSrcLc = mkRealSrcLoc (fsLit "<unknown file>") 0 0
      pstate = mkPState dflags buffer realSrcLc
  in case unP parseIdentifier pstate of
    POk _ (L _ name) -> Just (NsRdrName ns name)
    _ -> Nothing
