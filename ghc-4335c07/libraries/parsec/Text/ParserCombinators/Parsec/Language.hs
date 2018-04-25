-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Language
-- Copyright   :  (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Parsec compatibility module
--
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Language
    ( haskellDef,
      haskell,
      mondrianDef,
      mondrian,
      emptyDef,
      haskellStyle,
      javaStyle,
      LanguageDef,
      GenLanguageDef(..),
    ) where

import Text.Parsec.Token
import Text.Parsec.Language
