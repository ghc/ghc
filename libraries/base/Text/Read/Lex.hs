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
  ( Lexeme(..)       -- :: *; Show, Eq
  
  -- lexer
  , lex              -- :: ReadP Lexeme	-- Skips leading spaces
  , hsLex	     -- :: ReadP String
  
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
import GHC.Real( Ratio(..), Integral, Rational, (%), fromIntegral, fromRational, 
		 toInteger, (^), (^^), infinity, notANumber )
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
  = Char   Char		-- Quotes removed, 
  | String String	-- 	escapes interpreted
  | Punc   String 	-- Punctuation, eg "(", "::"
  | Ident  String	-- Haskell identifiers, e.g. foo, baz
  | Symbol String	-- Haskell symbols, e.g. >>, %
  | Int Integer
  | Rat Rational
  | EOF
 deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- Lexing

lex :: ReadP Lexeme
lex = skipSpaces >> lexToken

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
  isPuncChar c = c `elem` ",;()[]{}_`"

-- ----------------------------------------------------------------------
-- Symbols

lexSymbol :: ReadP Lexeme
lexSymbol =
  do s <- munch1 isSymbolChar
     if s `elem` reserved_ops then 
	return (Punc s)		-- Reserved-ops count as punctuation
      else
	return (Symbol s)
 where
  isSymbolChar c = c `elem` "!@#$%&*+./<=>?\\^|:-~"
  reserved_ops   = ["..", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

-- ----------------------------------------------------------------------
-- identifiers

lexId :: ReadP Lexeme
lexId =
  do c <- satisfy isAlpha
     s <- munch isIdfChar
     return (Ident (c:s))
 where
  isIdfChar c = isAlphaNum c || c `elem` "_'"

-- ---------------------------------------------------------------------------
-- Lexing character literals

lexLitChar :: ReadP Lexeme
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
         [ do { string "SO" ; s <- look; 
		case s of
		  'H' : _ -> do { get ; return '\SOH' }
		  other   -> return '\SO' 
	      }
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
  do char '"'
     body id
 where
  body f =
    do (c,esc) <- lexStrItem
       if c /= '"' || esc
         then body (f.(c:))
         else let s = f "" in
	      return (String s)

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
--  Lexing numbers

type Base   = Int
type Digits = [Int]

showDigit :: Int -> ShowS
showDigit n | n <= 9    = shows n
            | otherwise = showChar (chr (n + ord 'A' - 10))

lexNumber :: ReadP Lexeme
lexNumber = do { string "NaN";      return (Rat notANumber) } +++
	    do { string "Infinity"; return (Rat infinity) } +++
	    do { base <- lexBase ;  lexNumberBase base }
 where
  lexBase =
    do s <- look
       case s of
         '0':'o':_ -> do get; get; return 8
         '0':'O':_ -> do get; get; return 8
         '0':'x':_ -> do get; get; return 16
         '0':'X':_ -> do get; get; return 16
         _         -> do return 10
       
lexNumberBase :: Base -> ReadP Lexeme
lexNumberBase base =
  do xs    <- lexDigits base
     mFrac <- lexFrac base
     mExp  <- lexExp base
     return (value xs mFrac mExp)
 where
  baseInteger :: Integer
  baseInteger = fromIntegral base

  value xs mFrac mExp = valueFracExp (val baseInteger 0 xs) mFrac mExp
  
  valueFracExp :: Integer -> Maybe Digits -> Maybe Integer 
	       -> Lexeme
  valueFracExp a Nothing Nothing	
    = Int a						-- 43
  valueFracExp a Nothing (Just exp)
    | exp >= 0  = Int (a * (baseInteger ^ exp))		-- 43e7
    | otherwise = Rat (valExp (fromInteger a) exp)	-- 43e-7
  valueFracExp a (Just fs) mExp 
     = case mExp of
	 Nothing  -> Rat rat				-- 4.3
	 Just exp -> Rat (valExp rat exp)		-- 4.3e-4
     where
	rat :: Rational
	rat = fromInteger a + frac (fromIntegral base) 0 1 fs

  valExp :: Rational -> Integer -> Rational
  valExp rat exp = rat * (fromIntegral base ^^ exp)

lexFrac :: Base -> ReadP (Maybe Digits)
lexFrac base =
  do s <- look
     case s of
       '.' : d : _ | isJust (valDig base d) ->
	-- The lookahead checks for point and at least one
	-- valid following digit.  For example 1..n must
	-- lex the "1" off rather than failing.
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
-- Lex a non-empty sequence of digits in specified base
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
-- val base y [d1,..,dn] = y ++ [d1,..,dn], as it were
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
