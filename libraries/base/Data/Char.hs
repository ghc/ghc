{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}

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

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Arr (Ix)
import GHC.Char
import GHC.Real (fromIntegral)
import GHC.Show
import GHC.Read (Read, readLitChar, lexLitChar)
import GHC.Unicode
import GHC.Num
import GHC.Enum
#endif

-- | Convert a single digit 'Char' to the corresponding 'Int'.  
-- This function fails unless its argument satisfies 'isHexDigit',
-- but recognises both upper and lower-case hexadecimal digits
-- (i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@).
digitToInt :: Char -> Int
digitToInt c
 | isDigit c            =  ord c - ord '0'
 | c >= 'a' && c <= 'f' =  ord c - ord 'a' + 10
 | c >= 'A' && c <= 'F' =  ord c - ord 'A' + 10
 | otherwise            =  error ("Char.digitToInt: not a digit " ++ show c) -- sigh

#ifndef __GLASGOW_HASKELL__
isAsciiUpper, isAsciiLower :: Char -> Bool
isAsciiLower c          =  c >= 'a' && c <= 'z'
isAsciiUpper c          =  c >= 'A' && c <= 'Z'
#endif

-- | Unicode General Categories (column 2 of the UnicodeData table)
-- in the order they are listed in the Unicode standard.

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

-- | The Unicode general category of the character.
generalCategory :: Char -> GeneralCategory
#if defined(__GLASGOW_HASKELL__)
generalCategory c = toEnum $ fromIntegral $ wgencat $ fromIntegral $ ord c
#endif

-- derived character classifiers

-- | Selects alphabetic Unicode characters (lower-case, upper-case and
-- title-case letters, plus letters of caseless scripts and modifiers letters).
-- This function is equivalent to 'Data.Char.isAlpha'.
isLetter :: Char -> Bool
isLetter c = case generalCategory c of
        UppercaseLetter         -> True
        LowercaseLetter         -> True
        TitlecaseLetter         -> True
        ModifierLetter          -> True
        OtherLetter             -> True
        _                       -> False

-- | Selects Unicode mark characters, e.g. accents and the like, which
-- combine with preceding letters.
isMark :: Char -> Bool
isMark c = case generalCategory c of
        NonSpacingMark          -> True
        SpacingCombiningMark    -> True
        EnclosingMark           -> True
        _                       -> False

-- | Selects Unicode numeric characters, including digits from various
-- scripts, Roman numerals, etc.
isNumber :: Char -> Bool
isNumber c = case generalCategory c of
        DecimalNumber           -> True
        LetterNumber            -> True
        OtherNumber             -> True
        _                       -> False

-- | Selects Unicode punctuation characters, including various kinds
-- of connectors, brackets and quotes.
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
isSymbol :: Char -> Bool
isSymbol c = case generalCategory c of
        MathSymbol              -> True
        CurrencySymbol          -> True
        ModifierSymbol          -> True
        OtherSymbol             -> True
        _                       -> False

-- | Selects Unicode space and separator characters.
isSeparator :: Char -> Bool
isSeparator c = case generalCategory c of
        Space                   -> True
        LineSeparator           -> True
        ParagraphSeparator      -> True
        _                       -> False

