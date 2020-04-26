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

import GHC.Driver.Session ( DynFlags )
import GHC.Data.FastString   ( fsLit )
import GHC.Parser.Lexer ( mkPState, unP, ParseResult(POk) )
import GHC.Parser       ( parseIdentifier )
import GHC.Types.Name.Reader ( RdrName )
import GHC.Types.SrcLoc ( mkRealSrcLoc, unLoc )
import GHC.Data.StringBuffer ( stringToStringBuffer )

parseParas :: DynFlags -> Maybe Package -> String -> MetaDoc mod RdrName
parseParas d p = overDoc (P.overIdentifier (parseIdent d)) . P.parseParas p

parseString :: DynFlags -> String -> DocH mod RdrName
parseString d = P.overIdentifier (parseIdent d) . P.parseString

parseIdent :: DynFlags -> String -> Maybe RdrName
parseIdent dflags str0 =
  let buffer = stringToStringBuffer str0
      realSrcLc = mkRealSrcLoc (fsLit "<unknown file>") 0 0
      pstate = mkPState dflags buffer realSrcLc
  in case unP parseIdentifier pstate of
    POk _ name -> Just (unLoc name)
    _ -> Nothing
