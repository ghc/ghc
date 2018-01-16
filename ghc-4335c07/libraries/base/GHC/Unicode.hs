{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Unicode
-- Copyright   :  (c) The University of Glasgow, 2003
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Implementations for the character predicates (isLower, isUpper, etc.)
-- and the conversions (toUpper, toLower).  The implementation uses
-- libunicode on Unix systems if that is available.
--
-----------------------------------------------------------------------------

module GHC.Unicode (
        GeneralCategory (..), generalCategory,
        isAscii, isLatin1, isControl,
        isAsciiUpper, isAsciiLower,
        isPrint, isSpace,  isUpper,
        isLower, isAlpha,  isDigit,
        isOctDigit, isHexDigit, isAlphaNum,
        isPunctuation, isSymbol,
        toUpper, toLower, toTitle,
        wgencat
    ) where

import GHC.Base
import GHC.Char        (chr)
import GHC.Real
import GHC.Enum ( Enum (..), Bounded (..) )
import GHC.Arr ( Ix (..) )
import GHC.Num

-- Data.Char.chr already imports this and we need to define a Show instance
-- for GeneralCategory
import GHC.Show ( Show )

#include "HsBaseConfig.h"

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
        deriving (Show, Eq, Ord, Enum, Bounded, Ix)

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

-- | Selects the first 128 characters of the Unicode character set,
-- corresponding to the ASCII character set.
isAscii                 :: Char -> Bool
isAscii c               =  c <  '\x80'

-- | Selects the first 256 characters of the Unicode character set,
-- corresponding to the ISO 8859-1 (Latin-1) character set.
isLatin1                :: Char -> Bool
isLatin1 c              =  c <= '\xff'

-- | Selects ASCII lower-case letters,
-- i.e. characters satisfying both 'isAscii' and 'isLower'.
isAsciiLower :: Char -> Bool
isAsciiLower c          =  c >= 'a' && c <= 'z'

-- | Selects ASCII upper-case letters,
-- i.e. characters satisfying both 'isAscii' and 'isUpper'.
isAsciiUpper :: Char -> Bool
isAsciiUpper c          =  c >= 'A' && c <= 'Z'

-- | Selects control characters, which are the non-printing characters of
-- the Latin-1 subset of Unicode.
isControl               :: Char -> Bool

-- | Selects printable Unicode characters
-- (letters, numbers, marks, punctuation, symbols and spaces).
isPrint                 :: Char -> Bool

-- | Returns 'True' for any Unicode space character, and the control
-- characters @\\t@, @\\n@, @\\r@, @\\f@, @\\v@.
isSpace                 :: Char -> Bool
-- isSpace includes non-breaking space
-- The magic 0x377 isn't really that magical. As of 2014, all the codepoints
-- at or below 0x377 have been assigned, so we shouldn't have to worry about
-- any new spaces appearing below there. It would probably be best to
-- use branchless ||, but currently the eqLit transformation will undo that,
-- so we'll do it like this until there's a way around that.
isSpace c
  | uc <= 0x377 = uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
  | otherwise = iswspace (ord c) /= 0
  where
    uc = fromIntegral (ord c) :: Word

-- | Selects upper-case or title-case alphabetic Unicode characters (letters).
-- Title case is used by a small number of letter ligatures like the
-- single-character form of /Lj/.
isUpper                 :: Char -> Bool

-- | Selects lower-case alphabetic Unicode characters (letters).
isLower                 :: Char -> Bool

-- | Selects alphabetic Unicode characters (lower-case, upper-case and
-- title-case letters, plus letters of caseless scripts and modifiers letters).
-- This function is equivalent to 'Data.Char.isLetter'.
isAlpha                 :: Char -> Bool

-- | Selects alphabetic or numeric digit Unicode characters.
--
-- Note that numeric digits outside the ASCII range are selected by this
-- function but not by 'isDigit'.  Such digits may be part of identifiers
-- but are not used by the printer and reader to represent numbers.
isAlphaNum              :: Char -> Bool

-- | Selects ASCII digits, i.e. @\'0\'@..@\'9\'@.
isDigit                 :: Char -> Bool
isDigit c               =  (fromIntegral (ord c - ord '0') :: Word) <= 9

-- We use an addition and an unsigned comparison instead of two signed
-- comparisons because it's usually faster and puts less strain on branch
-- prediction. It likely also enables some CSE when combined with functions
-- that follow up with an actual conversion.

-- | Selects ASCII octal digits, i.e. @\'0\'@..@\'7\'@.
isOctDigit              :: Char -> Bool
isOctDigit c            =  (fromIntegral (ord c - ord '0') :: Word) <= 7

-- | Selects ASCII hexadecimal digits,
-- i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
isHexDigit              :: Char -> Bool
isHexDigit c            =  isDigit c ||
                           (fromIntegral (ord c - ord 'A')::Word) <= 5 ||
                           (fromIntegral (ord c - ord 'a')::Word) <= 5

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

-- | Convert a letter to the corresponding upper-case letter, if any.
-- Any other character is returned unchanged.
toUpper                 :: Char -> Char

-- | Convert a letter to the corresponding lower-case letter, if any.
-- Any other character is returned unchanged.
toLower                 :: Char -> Char

-- | Convert a letter to the corresponding title-case or upper-case
-- letter, if any.  (Title case differs from upper case only for a small
-- number of ligature letters.)
-- Any other character is returned unchanged.
toTitle                 :: Char -> Char

-- -----------------------------------------------------------------------------
-- Implementation with the supplied auto-generated Unicode character properties
-- table

-- Regardless of the O/S and Library, use the functions contained in WCsubst.c

isAlpha    c = iswalpha (ord c) /= 0
isAlphaNum c = iswalnum (ord c) /= 0
isControl  c = iswcntrl (ord c) /= 0
isPrint    c = iswprint (ord c) /= 0
isUpper    c = iswupper (ord c) /= 0
isLower    c = iswlower (ord c) /= 0

toLower c = chr (towlower (ord c))
toUpper c = chr (towupper (ord c))
toTitle c = chr (towtitle (ord c))

foreign import ccall unsafe "u_iswalpha"
  iswalpha :: Int -> Int

foreign import ccall unsafe "u_iswalnum"
  iswalnum :: Int -> Int

foreign import ccall unsafe "u_iswcntrl"
  iswcntrl :: Int -> Int

foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int

foreign import ccall unsafe "u_iswprint"
  iswprint :: Int -> Int

foreign import ccall unsafe "u_iswlower"
  iswlower :: Int -> Int

foreign import ccall unsafe "u_iswupper"
  iswupper :: Int -> Int

foreign import ccall unsafe "u_towlower"
  towlower :: Int -> Int

foreign import ccall unsafe "u_towupper"
  towupper :: Int -> Int

foreign import ccall unsafe "u_towtitle"
  towtitle :: Int -> Int

foreign import ccall unsafe "u_gencat"
  wgencat :: Int -> Int

