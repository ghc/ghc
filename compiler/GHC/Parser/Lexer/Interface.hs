{-# LANGUAGE MagicHash #-}

{- |
This module defines the types and functions necessary for an Alex-generated
lexer.

https://haskell-alex.readthedocs.io/en/latest/api.html#
-}
module GHC.Parser.Lexer.Interface (
  AlexInput (..),
  alexGetByte,
  alexInputPrevChar,

  -- * Helpers
  alexGetChar,
  adjustChar,
) where

import GHC.Prelude

import Data.Char (GeneralCategory (..), generalCategory, ord)
import Data.Word (Word8)
import GHC.Data.StringBuffer (StringBuffer, atEnd, nextChar, prevChar)
import GHC.Exts
import GHC.Types.SrcLoc (PsLoc, advancePsLoc)

data AlexInput = AI !PsLoc !StringBuffer deriving (Show)

-- See Note [Unicode in Alex]
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AI loc s)
  | atEnd s   = Nothing
  | otherwise = byte `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (byte, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advancePsLoc loc c
        byte   = adjustChar c

-- Getting the previous 'Char' isn't enough here - we need to convert it into
-- the same format that 'alexGetByte' would have produced.
--
-- See Note [Unicode in Alex] and #13986.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ buf) = unsafeChr (fromIntegral (adjustChar pc))
  where pc = prevChar buf '\n'

-- backwards compatibility for Alex 2.x
alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar inp = case alexGetByte inp of
                    Nothing    -> Nothing
                    Just (b,i) -> c `seq` Just (c,i)
                       where c = unsafeChr $ fromIntegral b

unsafeChr :: Int -> Char
unsafeChr (I# c) = GHC.Exts.C# (GHC.Exts.chr# c)

{-
Note [Unicode in Alex]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Although newer versions of Alex support unicode, this grammar is processed with
the old style '--latin1' behaviour. This means that when implementing the
functions

    alexGetByte       :: AlexInput -> Maybe (Word8,AlexInput)
    alexInputPrevChar :: AlexInput -> Char

which Alex uses to take apart our 'AlexInput', we must

  * return a latin1 character in the 'Word8' that 'alexGetByte' expects
  * return a latin1 character in 'alexInputPrevChar'.

We handle this in 'adjustChar' by squishing entire classes of unicode
characters into single bytes.
-}

{-# INLINE adjustChar #-}
adjustChar :: Char -> Word8
adjustChar c = adj_c
  where non_graphic     = 0x00
        upper           = 0x01
        lower           = 0x02
        digit           = 0x03
        symbol          = 0x04
        space           = 0x05
        other_graphic   = 0x06
        uniidchar       = 0x07

        adj_c
          | c <= '\x07' = non_graphic
          | c <= '\x7f' = fromIntegral (ord c)
          -- Alex doesn't handle Unicode, so when Unicode
          -- character is encountered we output these values
          -- with the actual character value hidden in the state.
          | otherwise =
                -- NB: The logic behind these definitions is also reflected
                -- in "GHC.Utils.Lexeme"
                -- Any changes here should likely be reflected there.

                case generalCategory c of
                  UppercaseLetter       -> upper
                  LowercaseLetter       -> lower
                  TitlecaseLetter       -> upper
                  ModifierLetter        -> uniidchar -- see #10196
                  OtherLetter           -> lower -- see #1103
                  NonSpacingMark        -> uniidchar -- see #7650
                  SpacingCombiningMark  -> other_graphic
                  EnclosingMark         -> other_graphic
                  DecimalNumber         -> digit
                  LetterNumber          -> digit
                  OtherNumber           -> digit -- see #4373
                  ConnectorPunctuation  -> symbol
                  DashPunctuation       -> symbol
                  OpenPunctuation       -> other_graphic
                  ClosePunctuation      -> other_graphic
                  InitialQuote          -> other_graphic
                  FinalQuote            -> other_graphic
                  OtherPunctuation      -> symbol
                  MathSymbol            -> symbol
                  CurrencySymbol        -> symbol
                  ModifierSymbol        -> symbol
                  OtherSymbol           -> symbol
                  Space                 -> space
                  _other                -> non_graphic
