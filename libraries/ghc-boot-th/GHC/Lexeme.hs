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
        startsVarSymASCII, isVarSymChar, okSymChar
  ) where

import Prelude -- See note [Why do we import Prelude here?]
import Data.Char

-- | Is this character acceptable in a symbol (after the first char)?
-- See alexGetByte in GHC.Parser.Lexer
okSymChar :: Char -> Bool
okSymChar c
  | c `elem` "(),;[]`{}_\"'"
  = False
  | otherwise
  = case generalCategory c of
      ConnectorPunctuation -> True
      DashPunctuation      -> True
      OtherPunctuation     -> True
      MathSymbol           -> True
      CurrencySymbol       -> True
      ModifierSymbol       -> True
      OtherSymbol          -> True
      _                    -> False

startsVarSym, startsVarId, startsConSym, startsConId :: Char -> Bool
startsVarSym c = okSymChar c && c /= ':' -- Infix Ids
startsConSym c = c == ':'                -- Infix data constructors
startsVarId c  = c == '_' || case generalCategory c of  -- Ordinary Ids
  LowercaseLetter -> True
  OtherLetter     -> True   -- See #1103
  _               -> False
startsConId c  = isUpper c || c == '('  -- Ordinary type constructors and data constructors

startsVarSymASCII :: Char -> Bool
startsVarSymASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"

isVarSymChar :: Char -> Bool
isVarSymChar c = c == ':' || startsVarSym c
