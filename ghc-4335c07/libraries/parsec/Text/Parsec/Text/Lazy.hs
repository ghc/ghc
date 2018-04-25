-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.String
-- Copyright   :  (c) Antoine Latter 2011
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
--
-- Maintainer  :  aslatter@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Convenience definitions for working with lazy 'Text.Text'.
--
-----------------------------------------------------------------------------

module Text.Parsec.Text.Lazy
    ( Parser, GenParser
    ) where

import qualified Data.Text.Lazy as Text
import Text.Parsec.Prim

type Parser = Parsec Text.Text ()
type GenParser st = Parsec Text.Text st
