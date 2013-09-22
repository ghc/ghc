{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
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
-----------------------------------------------------------------------------

module Text.Read.Lex
  -- lexing types
  ( Lexeme(..)

  , numberToInteger, numberToFixed, numberToRational, numberToRangedRational

  -- lexer
  , lex, expect
  , hsLex
  , lexChar

  , readIntP
  , readOctP
  , readDecP
  , readHexP
  )
 where

import Text.ParserCombinators.ReadP

import GHC.Base
import GHC.Char
import GHC.Num( Num(..), Integer )
import GHC.Show( Show(..) )
import {-# SOURCE #-} GHC.Unicode ( isSpace, isAlpha, isAlphaNum )
import GHC.Real( Rational, (%), fromIntegral,
                 toInteger, (^) )
import GHC.List
import GHC.Enum( minBound, maxBound )
import Data.Maybe
import Control.Monad

-- -----------------------------------------------------------------------------
-- Lexing types

-- ^ Haskell lexemes.
data Lexeme
  = Char   Char         -- ^ Character literal
  | String String       -- ^ String literal, with escapes interpreted
  | Punc   String       -- ^ Punctuation or reserved symbol, e.g. @(@, @::@
  | Ident  String       -- ^ Haskell identifier, e.g. @foo@, @Baz@
  | Symbol String       -- ^ Haskell symbol, e.g. @>>@, @:%@
  | Number Number       -- ^ /Since: 4.6.0.0/
  | EOF
 deriving (Eq, Show)

-- | /Since: 4.6.0.0/
data Number = MkNumber Int              -- Base
                       Digits           -- Integral part
            | MkDecimal Digits          -- Integral part
                        (Maybe Digits)  -- Fractional part
                        (Maybe Integer) -- Exponent
 deriving (Eq, Show)

-- | /Since: 4.5.1.0/
numberToInteger :: Number -> Maybe Integer
numberToInteger (MkNumber base iPart) = Just (val (fromIntegral base) 0 iPart)
numberToInteger (MkDecimal iPart Nothing Nothing) = Just (val 10 0 iPart)
numberToInteger _ = Nothing

-- | /Since: 4.7.0.0/
numberToFixed :: Integer -> Number -> Maybe (Integer, Integer)
numberToFixed _ (MkNumber base iPart) = Just (val (fromIntegral base) 0 iPart, 0)
numberToFixed _ (MkDecimal iPart Nothing Nothing) = Just (val 10 0 iPart, 0)
numberToFixed p (MkDecimal iPart (Just fPart) Nothing)
    = let i = val 10 0 iPart
          f = val 10 0 (integerTake p (fPart ++ repeat 0))
          -- Sigh, we really want genericTake, but that's above us in
          -- the hierarchy, so we define our own version here (actually
          -- specialised to Integer)
          integerTake             :: Integer -> [a] -> [a]
          integerTake n _ | n <= 0 = []
          integerTake _ []        =  []
          integerTake n (x:xs)    =  x : integerTake (n-1) xs
      in Just (i, f)
numberToFixed _ _ = Nothing

-- This takes a floatRange, and if the Rational would be outside of
-- the floatRange then it may return Nothing. Not that it will not
-- /necessarily/ return Nothing, but it is good enough to fix the
-- space problems in #5688
-- Ways this is conservative:
-- * the floatRange is in base 2, but we pretend it is in base 10
-- * we pad the floateRange a bit, just in case it is very small
--   and we would otherwise hit an edge case
-- * We only worry about numbers that have an exponent. If they don't
--   have an exponent then the Rational won't be much larger than the
--   Number, so there is no problem
-- | /Since: 4.5.1.0/
numberToRangedRational :: (Int, Int) -> Number
                       -> Maybe Rational -- Nothing = Inf
numberToRangedRational (neg, pos) n@(MkDecimal iPart mFPart (Just exp))
    -- if exp is out of integer bounds,
    -- then the number is definitely out of range
    | exp > fromIntegral (maxBound :: Int) ||
      exp < fromIntegral (minBound :: Int)
    = Nothing
    | otherwise
    = let mFirstDigit = case dropWhile (0 ==) iPart of
                        iPart'@(_ : _) -> Just (length iPart')
                        [] -> case mFPart of
                              Nothing -> Nothing
                              Just fPart ->
                                  case span (0 ==) fPart of
                                  (_, []) -> Nothing
                                  (zeroes, _) ->
                                      Just (negate (length zeroes))
      in case mFirstDigit of
         Nothing -> Just 0
         Just firstDigit ->
             let firstDigit' = firstDigit + fromInteger exp
             in if firstDigit' > (pos + 3)
                then Nothing
                else if firstDigit' < (neg - 3)
                then Just 0
                else Just (numberToRational n)
numberToRangedRational _ n = Just (numberToRational n)

-- | /Since: 4.6.0.0/
numberToRational :: Number -> Rational
numberToRational (MkNumber base iPart) = val (fromIntegral base) 0 iPart % 1
numberToRational (MkDecimal iPart mFPart mExp)
 = let i = val 10 0 iPart
   in case (mFPart, mExp) of
      (Nothing, Nothing)     -> i % 1
      (Nothing, Just exp)
       | exp >= 0            -> (i * (10 ^ exp)) % 1
       | otherwise           -> i % (10 ^ (- exp))
      (Just fPart, Nothing)  -> fracExp 0   i fPart
      (Just fPart, Just exp) -> fracExp exp i fPart
      -- fracExp is a bit more efficient in calculating the Rational.
      -- Instead of calculating the fractional part alone, then
      -- adding the integral part and finally multiplying with
      -- 10 ^ exp if an exponent was given, do it all at once.

-- -----------------------------------------------------------------------------
-- Lexing

lex :: ReadP Lexeme
lex = skipSpaces >> lexToken

-- | /Since: 4.7.0.0/
expect :: Lexeme -> ReadP ()
expect lexeme = do { skipSpaces 
                   ; thing <- lexToken
                   ; if thing == lexeme then return () else pfail }

hsLex :: ReadP String
-- ^ Haskell lexer: returns the lexed string, rather than the lexeme
hsLex = do skipSpaces
           (s,_) <- gather lexToken
           return s

lexToken :: ReadP Lexeme
lexToken = lexEOF     +++
           lexLitChar +++
           lexString  +++
           lexPunc    +++
           lexSymbol  +++
           lexId      +++
           lexNumber


-- ----------------------------------------------------------------------
-- End of file
lexEOF :: ReadP Lexeme
lexEOF = do s <- look
            guard (null s)
            return EOF

-- ---------------------------------------------------------------------------
-- Single character lexemes

lexPunc :: ReadP Lexeme
lexPunc =
  do c <- satisfy isPuncChar
     return (Punc [c])
 where
  isPuncChar c = c `elem` ",;()[]{}`"

-- ----------------------------------------------------------------------
-- Symbols

lexSymbol :: ReadP Lexeme
lexSymbol =
  do s <- munch1 isSymbolChar
     if s `elem` reserved_ops then
        return (Punc s)         -- Reserved-ops count as punctuation
      else
        return (Symbol s)
 where
  isSymbolChar c = c `elem` "!@#$%&*+./<=>?\\^|:-~"
  reserved_ops   = ["..", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

-- ----------------------------------------------------------------------
-- identifiers

lexId :: ReadP Lexeme
lexId = do c <- satisfy isIdsChar
           s <- munch isIdfChar
           return (Ident (c:s))
  where
          -- Identifiers can start with a '_'
    isIdsChar c = isAlpha c || c == '_'
    isIdfChar c = isAlphaNum c || c `elem` "_'"

-- ---------------------------------------------------------------------------
-- Lexing character literals

lexLitChar :: ReadP Lexeme
lexLitChar =
  do _ <- char '\''
     (c,esc) <- lexCharE
     guard (esc || c /= '\'')   -- Eliminate '' possibility
     _ <- char '\''
     return (Char c)

lexChar :: ReadP Char
lexChar = do { (c,_) <- lexCharE; return c }

lexCharE :: ReadP (Char, Bool)  -- "escaped or not"?
lexCharE =
  do c1 <- get
     if c1 == '\\'
       then do c2 <- lexEsc; return (c2, True)
       else do return (c1, False)
 where
  lexEsc =
    lexEscChar
      +++ lexNumeric
        +++ lexCntrlChar
          +++ lexAscii

  lexEscChar =
    do c <- get
       case c of
         'a'  -> return '\a'
         'b'  -> return '\b'
         'f'  -> return '\f'
         'n'  -> return '\n'
         'r'  -> return '\r'
         't'  -> return '\t'
         'v'  -> return '\v'
         '\\' -> return '\\'
         '\"' -> return '\"'
         '\'' -> return '\''
         _    -> pfail

  lexNumeric =
    do base <- lexBaseChar <++ return 10
       n    <- lexInteger base
       guard (n <= toInteger (ord maxBound))
       return (chr (fromInteger n))

  lexCntrlChar =
    do _ <- char '^'
       c <- get
       case c of
         '@'  -> return '\^@'
         'A'  -> return '\^A'
         'B'  -> return '\^B'
         'C'  -> return '\^C'
         'D'  -> return '\^D'
         'E'  -> return '\^E'
         'F'  -> return '\^F'
         'G'  -> return '\^G'
         'H'  -> return '\^H'
         'I'  -> return '\^I'
         'J'  -> return '\^J'
         'K'  -> return '\^K'
         'L'  -> return '\^L'
         'M'  -> return '\^M'
         'N'  -> return '\^N'
         'O'  -> return '\^O'
         'P'  -> return '\^P'
         'Q'  -> return '\^Q'
         'R'  -> return '\^R'
         'S'  -> return '\^S'
         'T'  -> return '\^T'
         'U'  -> return '\^U'
         'V'  -> return '\^V'
         'W'  -> return '\^W'
         'X'  -> return '\^X'
         'Y'  -> return '\^Y'
         'Z'  -> return '\^Z'
         '['  -> return '\^['
         '\\' -> return '\^\'
         ']'  -> return '\^]'
         '^'  -> return '\^^'
         '_'  -> return '\^_'
         _    -> pfail

  lexAscii =
    do choice
         [ (string "SOH" >> return '\SOH') <++
           (string "SO"  >> return '\SO')
                -- \SO and \SOH need maximal-munch treatment
                -- See the Haskell report Sect 2.6

         , string "NUL" >> return '\NUL'
         , string "STX" >> return '\STX'
         , string "ETX" >> return '\ETX'
         , string "EOT" >> return '\EOT'
         , string "ENQ" >> return '\ENQ'
         , string "ACK" >> return '\ACK'
         , string "BEL" >> return '\BEL'
         , string "BS"  >> return '\BS'
         , string "HT"  >> return '\HT'
         , string "LF"  >> return '\LF'
         , string "VT"  >> return '\VT'
         , string "FF"  >> return '\FF'
         , string "CR"  >> return '\CR'
         , string "SI"  >> return '\SI'
         , string "DLE" >> return '\DLE'
         , string "DC1" >> return '\DC1'
         , string "DC2" >> return '\DC2'
         , string "DC3" >> return '\DC3'
         , string "DC4" >> return '\DC4'
         , string "NAK" >> return '\NAK'
         , string "SYN" >> return '\SYN'
         , string "ETB" >> return '\ETB'
         , string "CAN" >> return '\CAN'
         , string "EM"  >> return '\EM'
         , string "SUB" >> return '\SUB'
         , string "ESC" >> return '\ESC'
         , string "FS"  >> return '\FS'
         , string "GS"  >> return '\GS'
         , string "RS"  >> return '\RS'
         , string "US"  >> return '\US'
         , string "SP"  >> return '\SP'
         , string "DEL" >> return '\DEL'
         ]


-- ---------------------------------------------------------------------------
-- string literal

lexString :: ReadP Lexeme
lexString =
  do _ <- char '"'
     body id
 where
  body f =
    do (c,esc) <- lexStrItem
       if c /= '"' || esc
         then body (f.(c:))
         else let s = f "" in
              return (String s)

  lexStrItem = (lexEmpty >> lexStrItem)
               +++ lexCharE

  lexEmpty =
    do _ <- char '\\'
       c <- get
       case c of
         '&'           -> do return ()
         _ | isSpace c -> do skipSpaces; _ <- char '\\'; return ()
         _             -> do pfail

-- ---------------------------------------------------------------------------
--  Lexing numbers

type Base   = Int
type Digits = [Int]

lexNumber :: ReadP Lexeme
lexNumber
  = lexHexOct  <++      -- First try for hex or octal 0x, 0o etc
                        -- If that fails, try for a decimal number
    lexDecNumber        -- Start with ordinary digits

lexHexOct :: ReadP Lexeme
lexHexOct
  = do  _ <- char '0'
        base <- lexBaseChar
        digits <- lexDigits base
        return (Number (MkNumber base digits))

lexBaseChar :: ReadP Int
-- Lex a single character indicating the base; fail if not there
lexBaseChar = do { c <- get;
                   case c of
                        'o' -> return 8
                        'O' -> return 8
                        'x' -> return 16
                        'X' -> return 16
                        _   -> pfail }

lexDecNumber :: ReadP Lexeme
lexDecNumber =
  do xs    <- lexDigits 10
     mFrac <- lexFrac <++ return Nothing
     mExp  <- lexExp  <++ return Nothing
     return (Number (MkDecimal xs mFrac mExp))

lexFrac :: ReadP (Maybe Digits)
-- Read the fractional part; fail if it doesn't
-- start ".d" where d is a digit
lexFrac = do _ <- char '.'
             fraction <- lexDigits 10
             return (Just fraction)

lexExp :: ReadP (Maybe Integer)
lexExp = do _ <- char 'e' +++ char 'E'
            exp <- signedExp +++ lexInteger 10
            return (Just exp)
 where
   signedExp
     = do c <- char '-' +++ char '+'
          n <- lexInteger 10
          return (if c == '-' then -n else n)

lexDigits :: Int -> ReadP Digits
-- Lex a non-empty sequence of digits in specified base
lexDigits base =
  do s  <- look
     xs <- scan s id
     guard (not (null xs))
     return xs
 where
  scan (c:cs) f = case valDig base c of
                    Just n  -> do _ <- get; scan cs (f.(n:))
                    Nothing -> do return (f [])
  scan []     f = do return (f [])

lexInteger :: Base -> ReadP Integer
lexInteger base =
  do xs <- lexDigits base
     return (val (fromIntegral base) 0 xs)

val :: Num a => a -> a -> Digits -> a
-- val base y [d1,..,dn] = y ++ [d1,..,dn], as it were
val _    y []     = y
val base y (x:xs) = y' `seq` val base y' xs
 where
  y' = y * base + fromIntegral x

-- Calculate a Rational from the exponent [of 10 to multiply with],
-- the integral part of the mantissa and the digits of the fractional
-- part. Leaving the calculation of the power of 10 until the end,
-- when we know the effective exponent, saves multiplications.
-- More importantly, this way we need at most one gcd instead of three.
--
-- frac was never used with anything but Integer and base 10, so
-- those are hardcoded now (trivial to change if necessary).
fracExp :: Integer -> Integer -> Digits -> Rational
fracExp exp mant []
  | exp < 0     = mant % (10 ^ (-exp))
  | otherwise   = fromInteger (mant * 10 ^ exp)
fracExp exp mant (d:ds) = exp' `seq` mant' `seq` fracExp exp' mant' ds
  where
    exp'  = exp - 1
    mant' = mant * 10 + fromIntegral d

valDig :: (Eq a, Num a) => a -> Char -> Maybe Int
valDig 8 c
  | '0' <= c && c <= '7' = Just (ord c - ord '0')
  | otherwise            = Nothing

valDig 10 c = valDecDig c

valDig 16 c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | 'a' <= c && c <= 'f' = Just (ord c - ord 'a' + 10)
  | 'A' <= c && c <= 'F' = Just (ord c - ord 'A' + 10)
  | otherwise            = Nothing

valDig _ _ = error "valDig: Bad base"

valDecDig :: Char -> Maybe Int
valDecDig c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | otherwise            = Nothing

-- ----------------------------------------------------------------------
-- other numeric lexing functions

readIntP :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadP a
readIntP base isDigit valDigit =
  do s <- munch1 isDigit
     return (val base 0 (map valDigit s))

readIntP' :: (Eq a, Num a) => a -> ReadP a
readIntP' base = readIntP base isDigit valDigit
 where
  isDigit  c = maybe False (const True) (valDig base c)
  valDigit c = maybe 0     id           (valDig base c)

readOctP, readDecP, readHexP :: (Eq a, Num a) => ReadP a
readOctP = readIntP' 8
readDecP = readIntP' 10
readHexP = readIntP' 16

