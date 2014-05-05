{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving
             , FlexibleInstances, UndecidableInstances
             , IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module      :  Haddock.Parser
-- Copyright   :  (c) Mateusz Kowalczyk 2013,
--                    Simon Hengel      2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable

module Haddock.Parser ( module Documentation.Haddock.Parser
                      , parseParasMaybe
                      , parseStringMaybe
                      , parseIdent
                      ) where

import Documentation.Haddock.Parser
import DynFlags (DynFlags)
import FastString (mkFastString)
import Documentation.Haddock.Types
import Lexer (mkPState, unP, ParseResult(POk))
import Parser (parseIdentifier)
import RdrName (RdrName)
import SrcLoc (mkRealSrcLoc, unLoc)
import StringBuffer (stringToStringBuffer)

{-# DEPRECATED parseParasMaybe "use `parseParas` instead" #-}
parseParasMaybe :: DynFlags -> String -> Maybe (DocH mod RdrName)
parseParasMaybe d = Just . overIdentifier (parseIdent d) . parseParas

{-# DEPRECATED parseStringMaybe "use `parseString` instead" #-}
parseStringMaybe :: DynFlags -> String -> Maybe (DocH mod RdrName)
parseStringMaybe d = Just . overIdentifier (parseIdent d) . parseString

parseIdent :: DynFlags -> String -> Maybe RdrName
parseIdent dflags str0 =
  let buffer = stringToStringBuffer str0
      realSrcLc = mkRealSrcLoc (mkFastString "<unknown file>") 0 0
      pstate = mkPState dflags buffer realSrcLc
  in case unP parseIdentifier pstate of
    POk _ name -> Just (unLoc name)
    _ -> Nothing
