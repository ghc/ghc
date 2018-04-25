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
-- Convinience definitions for working with 'Text.Text'.
--
-----------------------------------------------------------------------------

module Text.Parsec.Text
    ( Parser, GenParser
    ) where

import qualified Data.Text as Text
import Text.Parsec.Prim

type Parser = Parsec Text.Text ()
type GenParser st = Parsec Text.Text st
