{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Printf
-- Copyright   :  (c) Lennart Augustsson and Bart Massey 2013
-- License     :  BSD-style (see the file LICENSE in this distribution)
--
-- Maintainer  :  Bart Massey <bart@cs.pdx.edu>
-- Stability   :  provisional
-- Portability :  portable
--
-- A C @printf(3)@-like formatter. This version has been
-- extended by Bart Massey as per the recommendations of
-- John Meacham and Simon Marlow
-- <http://comments.gmane.org/gmane.comp.lang.haskell.libraries/4726>
-- to support extensible formatting for new datatypes.  It
-- has also been extended to support almost all C
-- @printf(3)@ syntax.
-----------------------------------------------------------------------------

module Text.Printf(
-- * Printing Functions
   printf, hPrintf,
-- * Extending To New Types
--
-- | This 'printf' can be extended to format types
-- other than those provided for by default. This
-- is done by instantiating 'PrintfArg' and providing
-- a 'formatArg' for the type. It is possible to
-- provide a 'parseFormat' to process type-specific
-- modifiers, but the default instance is usually
-- the best choice.
--
-- For example:
--
-- > instance PrintfArg () where
-- >   formatArg x fmt | fmtChar (vFmt 'U' fmt) == 'U' =
-- >     formatString "()" (fmt { fmtChar = 's', fmtPrecision = Nothing })
-- >   formatArg _ fmt = errorBadFormat $ fmtChar fmt
-- >
-- > main :: IO ()
-- > main = printf "[%-3.1U]\n" ()
--
-- prints \"@[() ]@\". Note the use of 'formatString' to
-- take care of field formatting specifications in a convenient
-- way.
   PrintfArg(..),
   FieldFormatter,
   FieldFormat(..),
   FormatAdjustment(..), FormatSign(..),
   vFmt,
-- ** Handling Type-specific Modifiers
--
-- | In the unlikely case that modifier characters of
-- some kind are desirable for a user-provided type,
-- a 'ModifierParser' can be provided to process these
-- characters. The resulting modifiers will appear in
-- the 'FieldFormat' for use by the type-specific formatter.
   ModifierParser, FormatParse(..),
-- ** Standard Formatters
--
-- | These formatters for standard types are provided for
-- convenience in writting new type-specific formatters:
-- a common pattern is to throw to 'formatString' or
-- 'formatInteger' to do most of the format handling for
-- a new type.
   formatString, formatChar, formatInt,
   formatInteger, formatRealFloat,
-- ** Raising Errors
--
-- | These functions are used internally to raise various
-- errors, and are exported for use by new type-specific
-- formatters.
  errorBadFormat, errorShortFormat, errorMissingArgument,
  errorBadArgument,
  perror,
-- * Implementation Internals
-- | These types are needed for implementing processing
-- variable numbers of arguments to 'printf' and 'hPrintf'.
-- Their implementation is intentionally not visible from
-- this module. If you attempt to pass an argument of a type
-- which is not an instance of the appropriate class to
-- 'printf' or 'hPrintf', then the compiler will report it
-- as a missing instance of 'PrintfArg'.  (All 'PrintfArg'
-- instances are 'PrintfType' instances.)
  PrintfType, HPrintfType,
-- | This class is needed as a Haskell98 compatibility
-- workaround for the lack of FlexibleInstances.
  IsChar(..)
) where

import Data.Char
import Data.Int
import Data.List (stripPrefix)
import Data.Word
import Numeric
import Numeric.Natural
import System.IO

-- $setup
-- >>> import Prelude

-------------------

-- | Format a variable number of arguments with the C-style formatting string.
--
-- >>> printf "%s, %d, %.4f" "hello" 123 pi
-- hello, 123, 3.1416
--
-- The return value is either 'String' or @('IO' a)@ (which
-- should be @('IO' ())@, but Haskell's type system
-- makes this hard).
--
-- The format string consists of ordinary characters and
-- /conversion specifications/, which specify how to format
-- one of the arguments to 'printf' in the output string. A
-- format specification is introduced by the @%@ character;
-- this character can be self-escaped into the format string
-- using @%%@. A format specification ends with a
-- /format character/ that provides the primary information about
-- how to format the value. The rest of the conversion
-- specification is optional.  In order, one may have flag
-- characters, a width specifier, a precision specifier, and
-- type-specific modifier characters.
--
-- Unlike C @printf(3)@, the formatting of this 'printf'
-- is driven by the argument type; formatting is type specific. The
-- types formatted by 'printf' \"out of the box\" are:
--
--   * 'Integral' types, including 'Char'
--
--   * 'String'
--
--   * 'RealFloat' types
--
-- 'printf' is also extensible to support other types: see below.
--
-- A conversion specification begins with the
-- character @%@, followed by zero or more of the following flags:
--
-- > -      left adjust (default is right adjust)
-- > +      always use a sign (+ or -) for signed conversions
-- > space  leading space for positive numbers in signed conversions
-- > 0      pad with zeros rather than spaces
-- > #      use an \"alternate form\": see below
--
-- When both flags are given, @-@ overrides @0@ and @+@ overrides space.
-- A negative width specifier in a @*@ conversion is treated as
-- positive but implies the left adjust flag.
--
-- The \"alternate form\" for unsigned radix conversions is
-- as in C @printf(3)@:
--
-- > %o           prefix with a leading 0 if needed
-- > %x           prefix with a leading 0x if nonzero
-- > %X           prefix with a leading 0X if nonzero
-- > %b           prefix with a leading 0b if nonzero
-- > %[eEfFgG]    ensure that the number contains a decimal point
--
-- Any flags are followed optionally by a field width:
--
-- > num    field width
-- > *      as num, but taken from argument list
--
-- The field width is a minimum, not a maximum: it will be
-- expanded as needed to avoid mutilating a value.
--
-- Any field width is followed optionally by a precision:
--
-- > .num   precision
-- > .      same as .0
-- > .*     as num, but taken from argument list
--
-- Negative precision is taken as 0. The meaning of the
-- precision depends on the conversion type.
--
-- > Integral    minimum number of digits to show
-- > RealFloat   number of digits after the decimal point
-- > String      maximum number of characters
--
-- The precision for Integral types is accomplished by zero-padding.
-- If both precision and zero-pad are given for an Integral field,
-- the zero-pad is ignored.
--
-- Any precision is followed optionally for Integral types
-- by a width modifier; the only use of this modifier being
-- to set the implicit size of the operand for conversion of
-- a negative operand to unsigned:
--
-- > hh     Int8
-- > h      Int16
-- > l      Int32
-- > ll     Int64
-- > L      Int64
--
-- The specification ends with a format character:
--
-- > c      character               Integral
-- > d      decimal                 Integral
-- > o      octal                   Integral
-- > x      hexadecimal             Integral
-- > X      hexadecimal             Integral
-- > b      binary                  Integral
-- > u      unsigned decimal        Integral
-- > f      floating point          RealFloat
-- > F      floating point          RealFloat
-- > g      general format float    RealFloat
-- > G      general format float    RealFloat
-- > e      exponent format float   RealFloat
-- > E      exponent format float   RealFloat
-- > s      string                  String
-- > v      default format          any type
--
-- The \"%v\" specifier is provided for all built-in types,
-- and should be provided for user-defined type formatters
-- as well. It picks a \"best\" representation for the given
-- type. For the built-in types the \"%v\" specifier is
-- converted as follows:
--
-- > c      Char
-- > u      other unsigned Integral
-- > d      other signed Integral
-- > g      RealFloat
-- > s      String
--
-- Mismatch between the argument types and the format
-- string, as well as any other syntactic or semantic errors
-- in the format string, will cause an exception to be
-- thrown at runtime.
--
-- Note that the formatting for 'RealFloat' types is
-- currently a bit different from that of C @printf(3)@,
-- conforming instead to 'Numeric.showEFloat',
-- 'Numeric.showFFloat' and 'Numeric.showGFloat' (and their
-- alternate versions 'Numeric.showFFloatAlt' and
-- 'Numeric.showGFloatAlt'). This is hard to fix: the fixed
-- versions would format in a backward-incompatible way.
-- In any case the Haskell behavior is generally more
-- sensible than the C behavior.  A brief summary of some
-- key differences:
--
-- * Haskell 'printf' never uses the default \"6-digit\" precision
--   used by C printf.
--
-- * Haskell 'printf' treats the \"precision\" specifier as
--   indicating the number of digits after the decimal point.
--
-- * Haskell 'printf' prints the exponent of e-format
--   numbers without a gratuitous plus sign, and with the
--   minimum possible number of digits.
--
-- * Haskell 'printf' will place a zero after a decimal point when
--   possible.
printf :: (PrintfType r) => String -> r
printf fmts = spr fmts []

-- | Similar to 'printf', except that output is via the specified
-- 'Handle'.  The return type is restricted to @('IO' a)@.
hPrintf :: (HPrintfType r) => Handle -> String -> r
hPrintf hdl fmts = hspr hdl fmts []

-- |The 'PrintfType' class provides the variable argument magic for
-- 'printf'.  Its implementation is intentionally not visible from
-- this module. If you attempt to pass an argument of a type which
-- is not an instance of this class to 'printf' or 'hPrintf', then
-- the compiler will report it as a missing instance of 'PrintfArg'.
class PrintfType t where
    spr :: String -> [UPrintf] -> t

-- | The 'HPrintfType' class provides the variable argument magic for
-- 'hPrintf'.  Its implementation is intentionally not visible from
-- this module.
class HPrintfType t where
    hspr :: Handle -> String -> [UPrintf] -> t

{- not allowed in Haskell 2010
instance PrintfType String where
    spr fmt args = uprintf fmt (reverse args)
-}
-- | @since 2.01
instance (IsChar c) => PrintfType [c] where
    spr fmts args = map fromChar (uprintf fmts (reverse args))

-- Note that this should really be (IO ()), but GHC's
-- type system won't readily let us say that without
-- bringing the GADTs. So we go conditional for these defs.

-- | @since 4.7.0.0
instance (a ~ ()) => PrintfType (IO a) where
    spr fmts args =
        putStr $ map fromChar $ uprintf fmts $ reverse args

-- | @since 4.7.0.0
instance (a ~ ()) => HPrintfType (IO a) where
    hspr hdl fmts args =
        hPutStr hdl (uprintf fmts (reverse args))

-- | @since 2.01
instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
    spr fmts args = \ a -> spr fmts
                             ((parseFormat a, formatArg a) : args)

-- | @since 2.01
instance (PrintfArg a, HPrintfType r) => HPrintfType (a -> r) where
    hspr hdl fmts args = \ a -> hspr hdl fmts
                                  ((parseFormat a, formatArg a) : args)

-- | Typeclass of 'printf'-formattable values. The 'formatArg' method
-- takes a value and a field format descriptor and either fails due
-- to a bad descriptor or produces a 'ShowS' as the result. The
-- default 'parseFormat' expects no modifiers: this is the normal
-- case. Minimal instance: 'formatArg'.
class PrintfArg a where
    -- | @since 4.7.0.0
    formatArg :: a -> FieldFormatter
    -- | @since 4.7.0.0
    parseFormat :: a -> ModifierParser
    parseFormat _ (c : cs) = FormatParse "" c cs
    parseFormat _ "" = errorShortFormat

-- | @since 2.01
instance PrintfArg Char where
    formatArg = formatChar
    parseFormat _ cf = parseIntFormat (undefined :: Int) cf

-- | @since 2.01
instance (IsChar c) => PrintfArg [c] where
    formatArg = formatString

-- | @since 2.01
instance PrintfArg Int where
    formatArg = formatInt
    parseFormat = parseIntFormat

-- | @since 2.01
instance PrintfArg Int8 where
    formatArg = formatInt
    parseFormat = parseIntFormat

-- | @since 2.01
instance PrintfArg Int16 where
    formatArg = formatInt
    parseFormat = parseIntFormat

-- | @since 2.01
instance PrintfArg Int32 where
    formatArg = formatInt
    parseFormat = parseIntFormat

-- | @since 2.01
instance PrintfArg Int64 where
    formatArg = formatInt
    parseFormat = parseIntFormat

-- | @since 2.01
instance PrintfArg Word where
    formatArg = formatInt
    parseFormat = parseIntFormat

-- | @since 2.01
instance PrintfArg Word8 where
    formatArg = formatInt
    parseFormat = parseIntFormat

-- | @since 2.01
instance PrintfArg Word16 where
    formatArg = formatInt
    parseFormat = parseIntFormat

-- | @since 2.01
instance PrintfArg Word32 where
    formatArg = formatInt
    parseFormat = parseIntFormat

-- | @since 2.01
instance PrintfArg Word64 where
    formatArg = formatInt
    parseFormat = parseIntFormat

-- | @since 2.01
instance PrintfArg Integer where
    formatArg = formatInteger
    parseFormat = parseIntFormat

-- | @since 4.8.0.0
instance PrintfArg Natural where
    formatArg = formatInteger . toInteger
    parseFormat = parseIntFormat

-- | @since 2.01
instance PrintfArg Float where
    formatArg = formatRealFloat

-- | @since 2.01
instance PrintfArg Double where
    formatArg = formatRealFloat

-- | This class, with only the one instance, is used as
-- a workaround for the fact that 'String', as a concrete
-- type, is not allowable as a typeclass instance. 'IsChar'
-- is exported for backward-compatibility.
class IsChar c where
    -- | @since 4.7.0.0
    toChar :: c -> Char
    -- | @since 4.7.0.0
    fromChar :: Char -> c

-- | @since 2.01
instance IsChar Char where
    toChar c = c
    fromChar c = c

-------------------

-- | Whether to left-adjust or zero-pad a field. These are
-- mutually exclusive, with 'LeftAdjust' taking precedence.
--
-- @since 4.7.0.0
data FormatAdjustment = LeftAdjust | ZeroPad

-- | How to handle the sign of a numeric field.  These are
-- mutually exclusive, with 'SignPlus' taking precedence.
--
-- @since 4.7.0.0
data FormatSign = SignPlus | SignSpace

-- | Description of field formatting for 'formatArg'. See UNIX @printf(3)@
-- for a description of how field formatting works.
--
-- @since 4.7.0.0
data FieldFormat = FieldFormat {
  fmtWidth :: Maybe Int,       -- ^ Total width of the field.
  fmtPrecision :: Maybe Int,   -- ^ Secondary field width specifier.
  fmtAdjust :: Maybe FormatAdjustment,  -- ^ Kind of filling or padding
                                        --   to be done.
  fmtSign :: Maybe FormatSign, -- ^ Whether to insist on a
                               -- plus sign for positive
                               -- numbers.
  fmtAlternate :: Bool,        -- ^ Indicates an "alternate
                               -- format".  See @printf(3)@
                               -- for the details, which
                               -- vary by argument spec.
  fmtModifiers :: String,      -- ^ Characters that appeared
                               -- immediately to the left of
                               -- 'fmtChar' in the format
                               -- and were accepted by the
                               -- type's 'parseFormat'.
                               -- Normally the empty string.
  fmtChar :: Char              -- ^ The format character
                               -- 'printf' was invoked
                               -- with. 'formatArg' should
                               -- fail unless this character
                               -- matches the type. It is
                               -- normal to handle many
                               -- different format
                               -- characters for a single
                               -- type.
  }

-- | The \"format parser\" walks over argument-type-specific
-- modifier characters to find the primary format character.
-- This is the type of its result.
--
-- @since 4.7.0.0
data FormatParse = FormatParse {
  fpModifiers :: String,   -- ^ Any modifiers found.
  fpChar :: Char,          -- ^ Primary format character.
  fpRest :: String         -- ^ Rest of the format string.
  }

-- Contains the "modifier letters" that can precede an
-- integer type.
intModifierMap :: [(String, Integer)]
intModifierMap = [
  ("hh", toInteger (minBound :: Int8)),
  ("h", toInteger (minBound :: Int16)),
  ("l", toInteger (minBound :: Int32)),
  ("ll", toInteger (minBound :: Int64)),
  ("L", toInteger (minBound :: Int64)) ]

parseIntFormat :: a -> String -> FormatParse
parseIntFormat _ s =
  case foldr matchPrefix Nothing intModifierMap of
    Just m -> m
    Nothing ->
      case s of
        c : cs -> FormatParse "" c cs
        "" -> errorShortFormat
  where
    matchPrefix (p, _) m@(Just (FormatParse p0 _ _))
      | length p0 >= length p = m
      | otherwise = case getFormat p of
          Nothing -> m
          Just fp -> Just fp
    matchPrefix (p, _) Nothing =
      getFormat p
    getFormat p =
      stripPrefix p s >>= fp
      where
        fp (c : cs) = Just $ FormatParse p c cs
        fp "" = errorShortFormat

-- | This is the type of a field formatter reified over its
-- argument.
--
-- @since 4.7.0.0
type FieldFormatter = FieldFormat -> ShowS

-- | Type of a function that will parse modifier characters
-- from the format string.
--
-- @since 4.7.0.0
type ModifierParser = String -> FormatParse

-- | Substitute a \'v\' format character with the given
-- default format character in the 'FieldFormat'. A
-- convenience for user-implemented types, which should
-- support \"%v\".
--
-- @since 4.7.0.0
vFmt :: Char -> FieldFormat -> FieldFormat
vFmt c ufmt@(FieldFormat {fmtChar = 'v'}) = ufmt {fmtChar = c}
vFmt _ ufmt = ufmt

-- | Formatter for 'Char' values.
--
-- @since 4.7.0.0
formatChar :: Char -> FieldFormatter
formatChar x ufmt =
  formatIntegral (Just 0) (toInteger $ ord x) $ vFmt 'c' ufmt

-- | Formatter for 'String' values.
--
-- @since 4.7.0.0
formatString :: IsChar a => [a] -> FieldFormatter
formatString x ufmt =
  case fmtChar $ vFmt 's' ufmt of
    's' -> map toChar . (adjust ufmt ("", ts) ++)
           where
             ts = map toChar $ trunc $ fmtPrecision ufmt
               where
                 trunc Nothing = x
                 trunc (Just n) = take n x
    c   -> errorBadFormat c

-- Possibly apply the int modifiers to get a new
-- int width for conversion.
fixupMods :: FieldFormat -> Maybe Integer -> Maybe Integer
fixupMods ufmt m =
  let mods = fmtModifiers ufmt in
  case mods of
    "" -> m
    _ -> case lookup mods intModifierMap of
      Just m0 -> Just m0
      Nothing -> perror "unknown format modifier"

-- | Formatter for 'Int' values.
--
-- @since 4.7.0.0
formatInt :: (Integral a, Bounded a) => a -> FieldFormatter
formatInt x ufmt =
  let lb = toInteger $ minBound `asTypeOf` x
      m = fixupMods ufmt (Just lb)
      ufmt' = case lb of
        0 -> vFmt 'u' ufmt
        _ -> ufmt
  in
  formatIntegral m (toInteger x) ufmt'

-- | Formatter for 'Integer' values.
--
-- @since 4.7.0.0
formatInteger :: Integer -> FieldFormatter
formatInteger x ufmt =
  let m = fixupMods ufmt Nothing in
  formatIntegral m x ufmt

-- All formatting for integral types is handled
-- consistently.  The only difference is between Integer and
-- bounded types; this difference is handled by the 'm'
-- argument containing the lower bound.
formatIntegral :: Maybe Integer -> Integer -> FieldFormatter
formatIntegral m x ufmt0 =
  let prec = fmtPrecision ufmt0 in
  case fmtChar ufmt of
    'd' -> (adjustSigned ufmt (fmti prec x) ++)
    'i' -> (adjustSigned ufmt (fmti prec x) ++)
    'x' -> (adjust ufmt (fmtu 16 (alt "0x" x) prec m x) ++)
    'X' -> (adjust ufmt (upcase $ fmtu 16 (alt "0X" x) prec m x) ++)
    'b' -> (adjust ufmt (fmtu 2 (alt "0b" x) prec m x) ++)
    'o' -> (adjust ufmt (fmtu 8 (alt "0" x) prec m x) ++)
    'u' -> (adjust ufmt (fmtu 10 Nothing prec m x) ++)
    'c' | x >= fromIntegral (ord (minBound :: Char)) &&
          x <= fromIntegral (ord (maxBound :: Char)) &&
          fmtPrecision ufmt == Nothing &&
          fmtModifiers ufmt == "" ->
            formatString [chr $ fromIntegral x] (ufmt { fmtChar = 's' })
    'c' -> perror "illegal char conversion"
    c   -> errorBadFormat c
  where
    ufmt = vFmt 'd' $ case ufmt0 of
      FieldFormat { fmtPrecision = Just _, fmtAdjust = Just ZeroPad } ->
        ufmt0 { fmtAdjust = Nothing }
      _ -> ufmt0
    alt _ 0 = Nothing
    alt p _ = case fmtAlternate ufmt of
      True -> Just p
      False -> Nothing
    upcase (s1, s2) = (s1, map toUpper s2)

-- | Formatter for 'RealFloat' values.
--
-- @since 4.7.0.0
formatRealFloat :: RealFloat a => a -> FieldFormatter
formatRealFloat x ufmt =
  let c = fmtChar $ vFmt 'g' ufmt
      prec = fmtPrecision ufmt
      alt = fmtAlternate ufmt
  in
   case c of
     'e' -> (adjustSigned ufmt (dfmt c prec alt x) ++)
     'E' -> (adjustSigned ufmt (dfmt c prec alt x) ++)
     'f' -> (adjustSigned ufmt (dfmt c prec alt x) ++)
     'F' -> (adjustSigned ufmt (dfmt c prec alt x) ++)
     'g' -> (adjustSigned ufmt (dfmt c prec alt x) ++)
     'G' -> (adjustSigned ufmt (dfmt c prec alt x) ++)
     _   -> errorBadFormat c

-- This is the type carried around for arguments in
-- the varargs code.
type UPrintf = (ModifierParser, FieldFormatter)

-- Given a format string and a list of formatting functions
-- (the actual argument value having already been baked into
-- each of these functions before delivery), return the
-- actual formatted text string.
uprintf :: String -> [UPrintf] -> String
uprintf s us = uprintfs s us ""

-- This function does the actual work, producing a ShowS
-- instead of a string, for future expansion and for
-- misguided efficiency.
uprintfs :: String -> [UPrintf] -> ShowS
uprintfs ""       []       = id
uprintfs ""       (_:_)    = errorShortFormat
uprintfs ('%':'%':cs) us   = ('%' :) . uprintfs cs us
uprintfs ('%':_)  []       = errorMissingArgument
uprintfs ('%':cs) us@(_:_) = fmt cs us
uprintfs (c:cs)   us       = (c :) . uprintfs cs us

-- Given a suffix of the format string starting just after
-- the percent sign, and the list of remaining unprocessed
-- arguments in the form described above, format the portion
-- of the output described by this field description, and
-- then continue with 'uprintfs'.
fmt :: String -> [UPrintf] -> ShowS
fmt cs0 us0 =
  case getSpecs False False Nothing False cs0 us0 of
    (_, _, []) -> errorMissingArgument
    (ufmt, cs, (_, u) : us) -> u ufmt . uprintfs cs us

-- Given field formatting information, and a tuple
-- consisting of a prefix (for example, a minus sign) that
-- is supposed to go before the argument value and a string
-- representing the value, return the properly padded and
-- formatted result.
adjust :: FieldFormat -> (String, String) -> String
adjust ufmt (pre, str) =
  let naturalWidth = length pre + length str
      zero = case fmtAdjust ufmt of
        Just ZeroPad -> True
        _ -> False
      left = case fmtAdjust ufmt of
        Just LeftAdjust -> True
        _ -> False
      fill = case fmtWidth ufmt of
        Just width | naturalWidth < width ->
          let fillchar = if zero then '0' else ' ' in
          replicate (width - naturalWidth) fillchar
        _ -> ""
  in
   if left
   then pre ++ str ++ fill
   else if zero
        then pre ++ fill ++ str
        else fill ++ pre ++ str

-- For positive numbers with an explicit sign field ("+" or
-- " "), adjust accordingly.
adjustSigned :: FieldFormat -> (String, String) -> String
adjustSigned ufmt@(FieldFormat {fmtSign = Just SignPlus}) ("", str) =
  adjust ufmt ("+", str)
adjustSigned ufmt@(FieldFormat {fmtSign = Just SignSpace}) ("", str) =
  adjust ufmt (" ", str)
adjustSigned ufmt ps =
  adjust ufmt ps

-- Format a signed integer in the "default" fashion.
-- This will be subjected to adjust subsequently.
fmti :: Maybe Int -> Integer -> (String, String)
fmti prec i
  | i < 0 = ("-", integral_prec prec (show (-i)))
  | otherwise = ("", integral_prec prec (show i))

-- Format an unsigned integer in the "default" fashion.
-- This will be subjected to adjust subsequently.  The 'b'
-- argument is the base, the 'pre' argument is the prefix,
-- and the '(Just m)' argument is the implicit lower-bound
-- size of the operand for conversion from signed to
-- unsigned. Thus, this function will refuse to convert an
-- unbounded negative integer to an unsigned string.
fmtu :: Integer -> Maybe String -> Maybe Int -> Maybe Integer -> Integer
     -> (String, String)
fmtu b (Just pre) prec m i =
  let ("", s) = fmtu b Nothing prec m i in
  case pre of
    "0" -> case s of
      '0' : _ -> ("", s)
      _ -> (pre, s)
    _ -> (pre, s)
fmtu b Nothing prec0 m0 i0 =
  case fmtu' prec0 m0 i0 of
    Just s -> ("", s)
    Nothing -> errorBadArgument
  where
    fmtu' :: Maybe Int -> Maybe Integer -> Integer -> Maybe String
    fmtu' prec (Just m) i | i < 0 =
      fmtu' prec Nothing (-2 * m + i)
    fmtu' (Just prec) _ i | i >= 0 =
      fmap (integral_prec (Just prec)) $ fmtu' Nothing Nothing i
    fmtu' Nothing _ i | i >= 0 =
      Just $ showIntAtBase b intToDigit i ""
    fmtu' _ _ _ = Nothing


-- This is used by 'fmtu' and 'fmti' to zero-pad an
-- int-string to a required precision.
integral_prec :: Maybe Int -> String -> String
integral_prec Nothing integral = integral
integral_prec (Just 0) "0" = ""
integral_prec (Just prec) integral =
  replicate (prec - length integral) '0' ++ integral

stoi :: String -> (Int, String)
stoi cs =
  let (as, cs') = span isDigit cs in
  case as of
    "" -> (0, cs')
    _ -> (read as, cs')

-- Figure out the FormatAdjustment, given:
--   width, precision, left-adjust, zero-fill
adjustment :: Maybe Int -> Maybe a -> Bool -> Bool
           -> Maybe FormatAdjustment
adjustment w p l z =
  case w of
    Just n | n < 0 -> adjl p True z
    _ -> adjl p l z
  where
    adjl _ True _ = Just LeftAdjust
    adjl _ False True = Just ZeroPad
    adjl _ _ _ = Nothing

-- Parse the various format controls to get a format specification.
getSpecs :: Bool -> Bool -> Maybe FormatSign -> Bool -> String -> [UPrintf]
         -> (FieldFormat, String, [UPrintf])
getSpecs _ z s a ('-' : cs0) us = getSpecs True z s a cs0 us
getSpecs l z _ a ('+' : cs0) us = getSpecs l z (Just SignPlus) a cs0 us
getSpecs l z s a (' ' : cs0) us =
  getSpecs l z ss a cs0 us
  where
    ss = case s of
      Just SignPlus -> Just SignPlus
      _ -> Just SignSpace
getSpecs l _ s a ('0' : cs0) us = getSpecs l True s a cs0 us
getSpecs l z s _ ('#' : cs0) us = getSpecs l z s True cs0 us
getSpecs l z s a ('*' : cs0) us =
  let (us', n) = getStar us
      ((p, cs''), us'') = case cs0 of
        '.':'*':r ->
          let (us''', p') = getStar us' in ((Just p', r), us''')
        '.':r ->
          let (p', r') = stoi r in ((Just p', r'), us')
        _ ->
          ((Nothing, cs0), us')
      FormatParse ms c cs =
        case us'' of
          (ufmt, _) : _ -> ufmt cs''
          [] -> errorMissingArgument
  in
   (FieldFormat {
       fmtWidth = Just (abs n),
       fmtPrecision = p,
       fmtAdjust = adjustment (Just n) p l z,
       fmtSign = s,
       fmtAlternate = a,
       fmtModifiers = ms,
       fmtChar = c}, cs, us'')
getSpecs l z s a ('.' : cs0) us =
  let ((p, cs'), us') = case cs0 of
        '*':cs'' -> let (us'', p') = getStar us in ((p', cs''), us'')
        _ ->        (stoi cs0, us)
      FormatParse ms c cs =
        case us' of
          (ufmt, _) : _ -> ufmt cs'
          [] -> errorMissingArgument
  in
   (FieldFormat {
       fmtWidth = Nothing,
       fmtPrecision = Just p,
       fmtAdjust = adjustment Nothing (Just p) l z,
       fmtSign = s,
       fmtAlternate = a,
       fmtModifiers = ms,
       fmtChar = c}, cs, us')
getSpecs l z s a cs0@(c0 : _) us | isDigit c0 =
  let (n, cs') = stoi cs0
      ((p, cs''), us') = case cs' of
        '.' : '*' : r ->
          let (us'', p') = getStar us in ((Just p', r), us'')
        '.' : r ->
          let (p', r') = stoi r in ((Just p', r'), us)
        _ ->
          ((Nothing, cs'), us)
      FormatParse ms c cs =
        case us' of
          (ufmt, _) : _ -> ufmt cs''
          [] -> errorMissingArgument
  in
   (FieldFormat {
       fmtWidth = Just (abs n),
       fmtPrecision = p,
       fmtAdjust = adjustment (Just n) p l z,
       fmtSign = s,
       fmtAlternate = a,
       fmtModifiers = ms,
       fmtChar = c}, cs, us')
getSpecs l z s a cs0@(_ : _) us =
  let FormatParse ms c cs =
        case us of
          (ufmt, _) : _ -> ufmt cs0
          [] -> errorMissingArgument
  in
   (FieldFormat {
       fmtWidth = Nothing,
       fmtPrecision = Nothing,
       fmtAdjust = adjustment Nothing Nothing l z,
       fmtSign = s,
       fmtAlternate = a,
       fmtModifiers = ms,
       fmtChar = c}, cs, us)
getSpecs _ _ _ _ ""       _  =
  errorShortFormat

-- Process a star argument in a format specification.
getStar :: [UPrintf] -> ([UPrintf], Int)
getStar us =
  let ufmt = FieldFormat {
        fmtWidth = Nothing,
        fmtPrecision = Nothing,
        fmtAdjust = Nothing,
        fmtSign = Nothing,
        fmtAlternate = False,
        fmtModifiers = "",
        fmtChar = 'd' } in
  case us of
    [] -> errorMissingArgument
    (_, nu) : us' -> (us', read (nu ufmt ""))

-- Format a RealFloat value.
dfmt :: (RealFloat a) => Char -> Maybe Int -> Bool -> a -> (String, String)
dfmt c p a d =
  let caseConvert = if isUpper c then map toUpper else id
      showFunction = case toLower c of
        'e' -> showEFloat
        'f' -> if a then showFFloatAlt else showFFloat
        'g' -> if a then showGFloatAlt else showGFloat
        _   -> perror "internal error: impossible dfmt"
      result = caseConvert $ showFunction p d ""
  in
   case result of
     '-' : cs -> ("-", cs)
     cs       -> ("" , cs)


-- | Raises an 'error' with a printf-specific prefix on the
-- message string.
--
-- @since 4.7.0.0
perror :: String -> a
perror s = errorWithoutStackTrace $ "printf: " ++ s

-- | Calls 'perror' to indicate an unknown format letter for
-- a given type.
--
-- @since 4.7.0.0
errorBadFormat :: Char -> a
errorBadFormat c = perror $ "bad formatting char " ++ show c

errorShortFormat, errorMissingArgument, errorBadArgument :: a
-- | Calls 'perror' to indicate that the format string ended
-- early.
--
-- @since 4.7.0.0
errorShortFormat = perror "formatting string ended prematurely"
-- | Calls 'perror' to indicate that there is a missing
-- argument in the argument list.
--
-- @since 4.7.0.0
errorMissingArgument = perror "argument list ended prematurely"
-- | Calls 'perror' to indicate that there is a type
-- error or similar in the given argument.
--
-- @since 4.7.0.0
errorBadArgument = perror "bad argument"
