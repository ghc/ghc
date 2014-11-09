{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Char
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The Char type and associated operations.
--
-----------------------------------------------------------------------------

module Data.Char
    (
      Char

    -- * Character classification
    -- | Unicode characters are divided into letters, numbers, marks,
    -- punctuation, symbols, separators (including spaces) and others
    -- (including control characters).
    , isControl, isSpace
    , isLower, isUpper, isAlpha, isAlphaNum, isPrint
    , isDigit, isOctDigit, isHexDigit
    , isLetter, isMark, isNumber, isPunctuation, isSymbol, isSeparator

    -- ** Subranges
    , isAscii, isLatin1
    , isAsciiUpper, isAsciiLower

    -- ** Unicode general categories
    , GeneralCategory(..), generalCategory

    -- * Case conversion
    , toUpper, toLower, toTitle

    -- * Single digit characters
    , digitToInt
    , intToDigit

    -- * Numeric representations
    , ord
    , chr

    -- * String representations
    , showLitChar
    , lexLitChar
    , readLitChar
    ) where

import GHC.Base
import GHC.Arr (Ix)
import GHC.Char
import GHC.Real (fromIntegral)
import GHC.Show
import GHC.Read (Read, readLitChar, lexLitChar)
import GHC.Unicode
import GHC.Num
import GHC.Enum

-- $setup
-- Allow the use of Prelude in doctests.
-- >>> import Prelude

-- | Convert a single digit 'Char' to the corresponding 'Int'.  This
-- function fails unless its argument satisfies 'isHexDigit', but
-- recognises both upper- and lower-case hexadecimal digits (that
-- is, @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@).
--
-- ==== __Examples__
--
-- Characters @\'0\'@ through @\'9\'@ are converted properly to
-- @0..9@:
--
-- >>> map digitToInt ['0'..'9']
-- [0,1,2,3,4,5,6,7,8,9]
--
-- Both upper- and lower-case @\'A\'@ through @\'F\'@ are converted
-- as well, to @10..15@.
--
-- >>> map digitToInt ['a'..'f']
-- [10,11,12,13,14,15]
-- >>> map digitToInt ['A'..'F']
-- [10,11,12,13,14,15]
--
-- Anything else throws an exception:
--
-- >>> digitToInt 'G'
-- *** Exception: Char.digitToInt: not a digit 'G'
-- >>> digitToInt '♥'
-- *** Exception: Char.digitToInt: not a digit '\9829'
--
digitToInt :: Char -> Int
digitToInt c
  | (fromIntegral dec::Word) <= 9 = dec
  | (fromIntegral hexl::Word) <= 5 = hexl + 10
  | (fromIntegral hexu::Word) <= 5 = hexu + 10
  | otherwise = error ("Char.digitToInt: not a digit " ++ show c) -- sigh
  where
    dec = ord c - ord '0'
    hexl = ord c - ord 'a'
    hexu = ord c - ord 'A'

-- | Unicode General Categories (column 2 of the UnicodeData table) in
-- the order they are listed in the Unicode standard (the Unicode
-- Character Database, in particular).
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> :t OtherLetter
-- OtherLetter :: GeneralCategory
--
-- 'Eq' instance:
--
-- >>> UppercaseLetter == UppercaseLetter
-- True
-- >>> UppercaseLetter == LowercaseLetter
-- False
--
-- 'Ord' instance:
--
-- >>> NonSpacingMark <= MathSymbol
-- True
--
-- 'Enum' instance:
--
-- >>> enumFromTo ModifierLetter SpacingCombiningMark
-- [ModifierLetter,OtherLetter,NonSpacingMark,SpacingCombiningMark]
--
-- 'Read' instance:
--
-- >>> read "DashPunctuation" :: GeneralCategory
-- DashPunctuation
-- >>> read "17" :: GeneralCategory
-- *** Exception: Prelude.read: no parse
--
-- 'Show' instance:
--
-- >>> show EnclosingMark
-- "EnclosingMark"
--
-- 'Bounded' instance:
--
-- >>> minBound :: GeneralCategory
-- UppercaseLetter
-- >>> maxBound :: GeneralCategory
-- NotAssigned
--
-- 'Ix' instance:
--
--  >>> import Data.Ix ( index )
--  >>> index (OtherLetter,Control) FinalQuote
--  12
--  >>> index (OtherLetter,Control) Format
--  *** Exception: Error in array index
--
data GeneralCategory
        = UppercaseLetter       -- ^ Lu: Letter, Uppercase
        | LowercaseLetter       -- ^ Ll: Letter, Lowercase
        | TitlecaseLetter       -- ^ Lt: Letter, Titlecase
        | ModifierLetter        -- ^ Lm: Letter, Modifier
        | OtherLetter           -- ^ Lo: Letter, Other
        | NonSpacingMark        -- ^ Mn: Mark, Non-Spacing
        | SpacingCombiningMark  -- ^ Mc: Mark, Spacing Combining
        | EnclosingMark         -- ^ Me: Mark, Enclosing
        | DecimalNumber         -- ^ Nd: Number, Decimal
        | LetterNumber          -- ^ Nl: Number, Letter
        | OtherNumber           -- ^ No: Number, Other
        | ConnectorPunctuation  -- ^ Pc: Punctuation, Connector
        | DashPunctuation       -- ^ Pd: Punctuation, Dash
        | OpenPunctuation       -- ^ Ps: Punctuation, Open
        | ClosePunctuation      -- ^ Pe: Punctuation, Close
        | InitialQuote          -- ^ Pi: Punctuation, Initial quote
        | FinalQuote            -- ^ Pf: Punctuation, Final quote
        | OtherPunctuation      -- ^ Po: Punctuation, Other
        | MathSymbol            -- ^ Sm: Symbol, Math
        | CurrencySymbol        -- ^ Sc: Symbol, Currency
        | ModifierSymbol        -- ^ Sk: Symbol, Modifier
        | OtherSymbol           -- ^ So: Symbol, Other
        | Space                 -- ^ Zs: Separator, Space
        | LineSeparator         -- ^ Zl: Separator, Line
        | ParagraphSeparator    -- ^ Zp: Separator, Paragraph
        | Control               -- ^ Cc: Other, Control
        | Format                -- ^ Cf: Other, Format
        | Surrogate             -- ^ Cs: Other, Surrogate
        | PrivateUse            -- ^ Co: Other, Private Use
        | NotAssigned           -- ^ Cn: Other, Not Assigned
        deriving (Eq, Ord, Enum, Read, Show, Bounded, Ix)

-- | The Unicode general category of the character. This relies on the
-- 'Enum' instance of 'GeneralCategory', which must remain in the
-- same order as the categories are presented in the Unicode
-- standard.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> generalCategory 'a'
-- LowercaseLetter
-- >>> generalCategory 'A'
-- UppercaseLetter
-- >>> generalCategory '0'
-- DecimalNumber
-- >>> generalCategory '%'
-- OtherPunctuation
-- >>> generalCategory '♥'
-- OtherSymbol
-- >>> generalCategory '\31'
-- Control
-- >>> generalCategory ' '
-- Space
--
generalCategory :: Char -> GeneralCategory
generalCategory c = toEnum $ fromIntegral $ wgencat $ fromIntegral $ ord c

-- derived character classifiers

-- | Selects alphabetic Unicode characters (lower-case, upper-case and
-- title-case letters, plus letters of caseless scripts and
-- modifiers letters). This function is equivalent to
-- 'Data.Char.isAlpha'.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'UppercaseLetter'
-- * 'LowercaseLetter'
-- * 'TitlecaseLetter'
-- * 'ModifierLetter'
-- * 'OtherLetter'
--
-- These classes are defined in the
-- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table Unicode Character Database>,
-- part of the Unicode standard. The same document defines what is
-- and is not a \"Letter\".
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isLetter 'a'
-- True
-- >>> isLetter 'A'
-- True
-- >>> isLetter '0'
-- False
-- >>> isLetter '%'
-- False
-- >>> isLetter '♥'
-- False
-- >>> isLetter '\31'
-- False
--
-- Ensure that 'isLetter' and 'isAlpha' are equivalent.
--
-- >>> let chars = [(chr 0)..]
-- >>> let letters = map isLetter chars
-- >>> let alphas = map isAlpha chars
-- >>> letters == alphas
-- True
--
isLetter :: Char -> Bool
isLetter c = case generalCategory c of
        UppercaseLetter         -> True
        LowercaseLetter         -> True
        TitlecaseLetter         -> True
        ModifierLetter          -> True
        OtherLetter             -> True
        _                       -> False

-- | Selects Unicode mark characters, for example accents and the
-- like, which combine with preceding characters.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'NonSpacingMark'
-- * 'SpacingCombiningMark'
-- * 'EnclosingMark'
--
-- These classes are defined in the
-- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table Unicode Character Database>,
-- part of the Unicode standard. The same document defines what is
-- and is not a \"Mark\".
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isMark 'a'
-- False
-- >>> isMark '0'
-- False
--
-- Combining marks such as accent characters usually need to follow
-- another character before they become printable:
--
-- >>> map isMark "ò"
-- [False,True]
--
-- Puns are not necessarily supported:
--
-- >>> isMark '✓'
-- False
--
isMark :: Char -> Bool
isMark c = case generalCategory c of
        NonSpacingMark          -> True
        SpacingCombiningMark    -> True
        EnclosingMark           -> True
        _                       -> False

-- | Selects Unicode numeric characters, including digits from various
-- scripts, Roman numerals, et cetera.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'DecimalNumber'
-- * 'LetterNumber'
-- * 'OtherNumber'
--
-- These classes are defined in the
-- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table Unicode Character Database>,
-- part of the Unicode standard. The same document defines what is
-- and is not a \"Number\".
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isNumber 'a'
-- False
-- >>> isNumber '%'
-- False
-- >>> isNumber '3'
-- True
--
-- ASCII @\'0\'@ through @\'9\'@ are all numbers:
--
-- >>> and $ map isNumber ['0'..'9']
-- True
--
-- Unicode Roman numerals are \"numbers\" as well:
--
-- >>> isNumber 'Ⅸ'
-- True
--
isNumber :: Char -> Bool
isNumber c = case generalCategory c of
        DecimalNumber           -> True
        LetterNumber            -> True
        OtherNumber             -> True
        _                       -> False

-- | Selects Unicode punctuation characters, including various kinds
-- of connectors, brackets and quotes.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'ConnectorPunctuation'
-- * 'DashPunctuation'
-- * 'OpenPunctuation'
-- * 'ClosePunctuation'
-- * 'InitialQuote'
-- * 'FinalQuote'
-- * 'OtherPunctuation'
--
-- These classes are defined in the
-- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table Unicode Character Database>,
-- part of the Unicode standard. The same document defines what is
-- and is not a \"Punctuation\".
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isPunctuation 'a'
-- False
-- >>> isPunctuation '7'
-- False
-- >>> isPunctuation '♥'
-- False
-- >>> isPunctuation '"'
-- True
-- >>> isPunctuation '?'
-- True
-- >>> isPunctuation '—'
-- True
--
isPunctuation :: Char -> Bool
isPunctuation c = case generalCategory c of
        ConnectorPunctuation    -> True
        DashPunctuation         -> True
        OpenPunctuation         -> True
        ClosePunctuation        -> True
        InitialQuote            -> True
        FinalQuote              -> True
        OtherPunctuation        -> True
        _                       -> False

-- | Selects Unicode symbol characters, including mathematical and
-- currency symbols.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'MathSymbol'
-- * 'CurrencySymbol'
-- * 'ModifierSymbol'
-- * 'OtherSymbol'
--
-- These classes are defined in the
-- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table Unicode Character Database>,
-- part of the Unicode standard. The same document defines what is
-- and is not a \"Symbol\".
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isSymbol 'a'
-- False
-- >>> isSymbol '6'
-- False
-- >>> isSymbol '='
-- True
--
-- The definition of \"math symbol\" may be a little
-- counter-intuitive depending on one's background:
--
-- >>> isSymbol '+'
-- True
-- >>> isSymbol '-'
-- False
--
isSymbol :: Char -> Bool
isSymbol c = case generalCategory c of
        MathSymbol              -> True
        CurrencySymbol          -> True
        ModifierSymbol          -> True
        OtherSymbol             -> True
        _                       -> False

-- | Selects Unicode space and separator characters.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'Space'
-- * 'LineSeparator'
-- * 'ParagraphSeparator'
--
-- These classes are defined in the
-- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table Unicode Character Database>,
-- part of the Unicode standard. The same document defines what is
-- and is not a \"Separator\".
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isSeparator 'a'
-- False
-- >>> isSeparator '6'
-- False
-- >>> isSeparator ' '
-- True
--
-- Warning: newlines and tab characters are not considered
-- separators.
--
-- >>> isSeparator '\n'
-- False
-- >>> isSeparator '\t'
-- False
--
-- But some more exotic characters are (like HTML's @&nbsp;@):
--
-- >>> isSeparator '\160'
-- True
--
isSeparator :: Char -> Bool
isSeparator c = case generalCategory c of
        Space                   -> True
        LineSeparator           -> True
        ParagraphSeparator      -> True
        _                       -> False

