{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Text.Read.Lex
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (uses Text.ParserCombinators.ReadP)
--
-- The cut-down Haskell lexer, used by Text.Read
--

module Text.Read.Lex
    (Lexeme(..),
     Number,
     numberToInteger,
     numberToFixed,
     numberToRational,
     numberToRangedRational,
     lex,
     expect,
     hsLex,
     lexChar,
     readBinP,
     readIntP,
     readOctP,
     readDecP,
     readHexP,
     isSymbolChar
     ) where

import GHC.Internal.Text.Read.Lex