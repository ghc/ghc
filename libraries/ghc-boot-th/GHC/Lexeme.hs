-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Lexeme
-- Copyright   :  (c) The GHC Team
--
-- Maintainer  :  ghc-devs@haskell.org
-- Portability :  portable
--
-- Functions to evaluate whether or not a string is a valid identifier.
--
module GHC.Lexeme (
          -- * Lexical characteristics of Haskell names
        startsVarSym, startsVarId, startsConSym, startsConId,
        startsVarSymASCII, isVarSymChar
  ) where

import Data.Char

startsVarSym, startsVarId, startsConSym, startsConId :: Char -> Bool
startsVarSym c = startsVarSymASCII c || (ord c > 0x7f && isSymbol c)  -- Infix Ids
startsConSym c = c == ':'               -- Infix data constructors
startsVarId c  = c == '_' || case generalCategory c of  -- Ordinary Ids
  LowercaseLetter -> True
  OtherLetter     -> True   -- See #1103
  _               -> False
startsConId c  = isUpper c || c == '('  -- Ordinary type constructors and data constructors

startsVarSymASCII :: Char -> Bool
startsVarSymASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"

isVarSymChar :: Char -> Bool
isVarSymChar c = c == ':' || startsVarSym c
