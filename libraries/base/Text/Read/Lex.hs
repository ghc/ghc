{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Read.Lex
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The cut-down Haskell lexer, used by Text.Read
--
-----------------------------------------------------------------------------

module Text.Read.Lex
  -- lexing types
  ( LexP             -- :: *; = ReadP Lexeme
  , Lexeme(..)       -- :: *; Show, Eq
  
  -- lexer
  , lex              -- :: LexP
  , lexLitChar	     -- :: LexP
  
  -- numbers
  , Number           -- :: *; Show, Eq
  
  , numberToInt      -- :: Number -> Maybe Int
  , numberToInteger  -- :: Number -> Maybe Integer
  , numberToRational -- :: Number -> Maybe Integer
  , numberToFloat    -- :: Number -> Maybe Float
  , numberToDouble   -- :: Number -> Maybe Double

  , readIntP         -- :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadP a
  , readOctP         -- :: Num a => ReadP a 
  , readDecP         -- :: Num a => ReadP a
  , readHexP         -- :: Num a => ReadP a
  )
 where

import Text.ParserCombinators.ReadP

import GHC.Base
import GHC.Num( Num(..), Integer )
import GHC.Show( Show(.. ), showChar, showString,
		 isSpace, isAlpha, isAlphaNum,
		 isOctDigit, isHexDigit, toUpper )
import GHC.Real( Ratio, Integral, Rational, (%), fromIntegral, fromRational, 
		 toInteger, (^), (^^) )
import GHC.Float( Float, Double )
import GHC.List
import GHC.Show( ShowS, shows )
import GHC.Enum( minBound, maxBound )
import Data.Maybe
import Data.Either
import Control.Monad

-- -----------------------------------------------------------------------------
-- Lexing types

type LexP = ReadP Lexeme

data Lexeme
  = Char   Char
  | String String
  | Single Char
  | Symbol String
  | Ident  String
  | Number Number
 deriving (Eq)

instance Show Lexeme where
  showsPrec n (Char c)   = showsPrec n c
  showsPrec n (String s) = showsPrec n s
  showsPrec _ (Single c) = showChar c
  showsPrec _ (Ident s)  = showString s
  showsPrec _ (Symbol s) = showString s
  showsPrec n (Number x) = showsPrec n x

-- -----------------------------------------------------------------------------
-- Lexing

lex :: LexP
lex =
  do skipSpaces
     (lexLitChar
       +++ lexString
         +++ lexSingle
           +++ lexSymbol
             +++ lexIdf
               +++ lexNumber)

-- ----------------------------------------------------------------------
-- symbols

lexSymbol :: LexP
lexSymbol =
  do s <- munch1 isSymbolChar
     return (Symbol s)
 where
  isSymbolChar c = c `elem` "!@#$%&*+./<=>?\\^|:-~"

-- ----------------------------------------------------------------------
-- identifiers

lexIdf :: LexP
lexIdf =
  do c <- satisfy isAlpha
     s <- munch isIdfChar
     return (Ident (c:s))
 where
  isIdfChar c = isAlphaNum c || c `elem` "_'"

-- ---------------------------------------------------------------------------
-- Lexing character literals

lexLitChar :: LexP
lexLitChar =
  do char '\''
     (c,esc) <- lexChar
     guard (esc || c /= '\'')
     char '\''
     return (Char c)

lexChar :: ReadP (Char, Bool)  -- "escaped or not"?
lexChar =
  do c <- get
     if c == '\\'
       then do c <- lexEsc; return (c, True)
       else do return (c, False)
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
    do base <- lexBase
       n    <- lexInteger base
       guard (n <= toInteger (ord maxBound))
       return (chr (fromInteger n))
   where
    lexBase =
      do s <- look
         case s of
           'o':_ -> do get; return 8
           'x':_ -> do get; return 16
           _     -> do return 10
  
  lexCntrlChar =
    do char '^'
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
         [ string "NUL" >> return '\NUL'
         , string "SOH" >> return '\SOH'
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
         , string "SO"  >> return '\SO'
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

lexString :: LexP
lexString =
  do char '"'
     body id
 where
  body f =
    do (c,esc) <- lexStrItem
       if c /= '"' || esc
         then body (f.(c:))
         else return (String (f ""))

  lexStrItem =
    (lexEmpty >> lexStrItem)
      +++ lexChar
  
  lexEmpty =
    do char '\\'
       c <- get
       case c of
         '&'           -> do return ()
         _ | isSpace c -> do skipSpaces; char '\\'; return ()
         _             -> do pfail

-- ---------------------------------------------------------------------------
-- single character lexemes

lexSingle :: LexP
lexSingle =
  do c <- satisfy isSingleChar
     return (Single c)
 where
  isSingleChar c = c `elem` ",;()[]{=}_`"

-- ---------------------------------------------------------------------------
--  Lexing numbers

data Number
  = MkNumber
    { value    :: Either Integer Rational
    , base     :: Base
    , digits   :: Digits
    , fraction :: Maybe Digits
    , exponent :: Maybe Integer
    }
 deriving (Eq)

type Base   = Int
type Digits = [Int]

instance Show Number where
  showsPrec _ x =
      showsBase (base x)
    . foldr (.) id (map showDigit (digits x))
    . showsFrac (fraction x)
    . showsExp (exponent x)
   where
    showsBase 8  = showString "0o"
    showsBase 10 = id
    showsBase 16 = showString "0x"
   
    showsFrac Nothing   = id
    showsFrac (Just ys) =
        showChar '.'
      . foldr (.) id (map showDigit ys) 
    
    showsExp Nothing    = id
    showsExp (Just exp) =
        showChar 'e'
      . shows exp

showDigit :: Int -> ShowS
showDigit n | n <= 9    = shows n
            | otherwise = showChar (chr (n + ord 'A' - 10))

lexNumber :: LexP
lexNumber =
  do base <- lexBase
     lexNumberBase base
 where
  lexBase =
    do s <- look
       case s of
         '0':'o':_ -> do get; get; return 8
         '0':'O':_ -> do get; get; return 8
         '0':'x':_ -> do get; get; return 16
         '0':'X':_ -> do get; get; return 16
         _         -> do return 10
       
lexNumberBase :: Base -> LexP
lexNumberBase base =
  do xs    <- lexDigits base
     mFrac <- lexFrac base
     mExp  <- lexExp base
     return (Number (MkNumber (value xs mFrac mExp) base xs mFrac mExp))
 where
  value xs mFrac mExp = valueFracExp (val (fromIntegral base) 0 xs) mFrac mExp
  
  valueFracExp a Nothing   mExp = Left (valueExp a mExp)
  valueFracExp a (Just fs) mExp =
    Right (valueExp (fromInteger a + frac (fromIntegral base) 0 1 fs) mExp)

  valueExp a Nothing    = a
  valueExp a (Just exp) = a * (fromIntegral base ^ exp)

lexFrac :: Base -> ReadP (Maybe Digits)
lexFrac base =
  do s <- look
     case s of
       '.' : _ ->
         do get
            frac <- lexDigits base
            return (Just frac)
       
       _ ->
         do return Nothing

lexExp :: Base -> ReadP (Maybe Integer)
lexExp base =
  do s <- look
     case s of
       e : _ | e `elem` "eE" && base == 10 ->
         do get
            (signedExp +++ exp)
        where
         signedExp =
           do c <- char '-' +++ char '+'
              n <- lexInteger 10
              return (Just (if c == '-' then -n else n))
         
         exp =
           do n <- lexInteger 10
              return (Just n)

       _ ->
         do return Nothing

lexDigits :: Int -> ReadP Digits
lexDigits base =
  do s  <- look
     xs <- scan s id
     guard (not (null xs))
     return xs
 where
  scan (c:cs) f = case valDig base c of
                    Just n  -> do get; scan cs (f.(n:))
                    Nothing -> do return (f [])
  scan []     f = do return (f [])

lexInteger :: Base -> ReadP Integer
lexInteger base =
  do xs <- lexDigits base
     return (val (fromIntegral base) 0 xs)

val :: Num a => a -> a -> Digits -> a
val base y []     = y
val base y (x:xs) = y' `seq` val base y' xs
 where
  y' = y * base + fromIntegral x

frac :: Integral a => a -> a -> a -> Digits -> Ratio a
frac base a b []     = a % b
frac base a b (x:xs) = a' `seq` b' `seq` frac base a' b' xs
 where
  a' = a * base + fromIntegral x
  b' = b * base

valDig :: Num a => a -> Char -> Maybe Int
valDig 8 c
  | '0' <= c && c <= '7' = Just (ord c - ord '0')
  | otherwise            = Nothing

valDig 10 c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | otherwise            = Nothing

valDig 16 c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | 'a' <= c && c <= 'f' = Just (ord c - ord 'a' + 10)
  | 'A' <= c && c <= 'F' = Just (ord c - ord 'A' + 10)
  | otherwise            = Nothing

-- ----------------------------------------------------------------------
-- conversion

numberToInt :: Number -> Maybe Int
numberToInt x =
  case numberToInteger x of
    Just n | minBound' <= n && n <= maxBound' -> Just (fromInteger n)
    _                                         -> Nothing
 where
  minBound' = toInteger (minBound :: Int)
  maxBound' = toInteger (maxBound :: Int)

numberToInteger :: Number -> Maybe Integer
numberToInteger x =
  case value x of
    Left n -> Just n
    _      -> Nothing

numberToRational :: Number -> Maybe Rational
numberToRational x =
  case value x of
    Left n  -> Just (fromInteger n)
    Right r -> Just r

numberToFloat :: Number -> Maybe Float
numberToFloat x =
  case value x of
    Left n  -> Just (fromInteger n)
    Right r -> Just (fromRational r)

numberToDouble :: Number -> Maybe Double
numberToDouble x =
  case value x of
    Left n  -> Just (fromInteger n)
    Right r -> Just (fromRational r)

-- ----------------------------------------------------------------------
-- other numeric lexing functions

readIntP :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadP a
readIntP base isDigit valDigit =
  do s <- munch1 isDigit
     return (val base 0 (map valDigit s))

readIntP' :: Num a => a -> ReadP a
readIntP' base = readIntP base isDigit valDigit
 where
  isDigit  c = maybe False (const True) (valDig base c)
  valDigit c = maybe 0     id           (valDig base c)

readOctP, readDecP, readHexP :: Num a => ReadP a
readOctP = readIntP' 8
readDecP = readIntP' 10
readHexP = readIntP' 16
