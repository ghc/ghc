{-# OPTIONS_GHC -fno-implicit-prelude #-}
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
  ( Lexeme(..)  -- :: *; Show, Eq
  		
  -- lexer	
  , lex         -- :: ReadP Lexeme	Skips leading spaces
  , hsLex	-- :: ReadP String
  , lexChar	-- :: ReadP Char	Reads just one char, with H98 escapes
  
  , readIntP    -- :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadP a
  , readOctP    -- :: Num a => ReadP a 
  , readDecP    -- :: Num a => ReadP a
  , readHexP    -- :: Num a => ReadP a
  )
 where

import Text.ParserCombinators.ReadP

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Num( Num(..), Integer )
import GHC.Show( Show(..) )
#ifndef __HADDOCK__
import {-# SOURCE #-} GHC.Unicode ( isSpace, isAlpha, isAlphaNum )
#endif
import GHC.Real( Ratio(..), Integral, Rational, (%), fromIntegral, 
		 toInteger, (^), (^^), infinity, notANumber )
import GHC.List
import GHC.Enum( maxBound )
#else
import Prelude hiding ( lex )
import Data.Char( chr, ord, isSpace, isAlpha, isAlphaNum )
import Data.Ratio( Ratio, (%) )
#endif
#ifdef __HUGS__
import Hugs.Prelude( Ratio(..) )
#endif
import Data.Maybe
import Control.Monad

-- -----------------------------------------------------------------------------
-- Lexing types

-- ^ Haskell lexemes.
data Lexeme
  = Char   Char		-- ^ Character literal
  | String String	-- ^ String literal, with escapes interpreted
  | Punc   String 	-- ^ Punctuation or reserved symbol, e.g. @(@, @::@
  | Ident  String	-- ^ Haskell identifier, e.g. @foo@, @Baz@
  | Symbol String	-- ^ Haskell symbol, e.g. @>>@, @:%@
  | Int Integer		-- ^ Integer literal
  | Rat Rational	-- ^ Floating point literal
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
  isPuncChar c = c `elem` ",;()[]{}`"

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
lexId = lex_nan <++ lex_id
  where
	-- NaN and Infinity look like identifiers, so
	-- we parse them first.  
    lex_nan = (string "NaN"      >> return (Rat notANumber)) +++
  	      (string "Infinity" >> return (Rat infinity))
  
    lex_id = do c <- satisfy isIdsChar
  		s <- munch isIdfChar
  		return (Ident (c:s))

  	  -- Identifiers can start with a '_'
    isIdsChar c = isAlpha c || c == '_'
    isIdfChar c = isAlphaNum c || c `elem` "_'"

#ifndef __GLASGOW_HASKELL__
infinity, notANumber :: Rational
infinity   = 1 :% 0
notANumber = 0 :% 0
#endif

-- ---------------------------------------------------------------------------
-- Lexing character literals

lexLitChar :: ReadP Lexeme
lexLitChar =
  do char '\''
     (c,esc) <- lexCharE
     guard (esc || c /= '\'')	-- Eliminate '' possibility
     char '\''
     return (Char c)

lexChar :: ReadP Char
lexChar = do { (c,_) <- lexCharE; return c }

lexCharE :: ReadP (Char, Bool)  -- "escaped or not"?
lexCharE =
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
    do base <- lexBaseChar <++ return 10
       n    <- lexInteger base
       guard (n <= toInteger (ord maxBound))
       return (chr (fromInteger n))

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
  do char '"'
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

lexNumber :: ReadP Lexeme
lexNumber 
  = lexHexOct  <++	-- First try for hex or octal 0x, 0o etc
			-- If that fails, try for a decimal number
    lexDecNumber 	-- Start with ordinary digits
		
lexHexOct :: ReadP Lexeme
lexHexOct
  = do	char '0'
	base <- lexBaseChar
	digits <- lexDigits base
	return (Int (val (fromIntegral base) 0 digits))

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
     return (value xs mFrac mExp)
 where
  value xs mFrac mExp = valueFracExp (val 10 0 xs) mFrac mExp
  
  valueFracExp :: Integer -> Maybe Digits -> Maybe Integer 
	       -> Lexeme
  valueFracExp a Nothing Nothing	
    = Int a						-- 43
  valueFracExp a Nothing (Just exp)
    | exp >= 0  = Int (a * (10 ^ exp))			-- 43e7
    | otherwise = Rat (valExp (fromInteger a) exp)	-- 43e-7
  valueFracExp a (Just fs) mExp 
     = case mExp of
	 Nothing  -> Rat rat				-- 4.3
	 Just exp -> Rat (valExp rat exp)		-- 4.3e-4
     where
	rat :: Rational
	rat = fromInteger a + frac 10 0 1 fs

  valExp :: Rational -> Integer -> Rational
  valExp rat exp = rat * (10 ^^ exp)

lexFrac :: ReadP (Maybe Digits)
-- Read the fractional part; fail if it doesn't
-- start ".d" where d is a digit
lexFrac = do char '.'
	     frac <- lexDigits 10
	     return (Just frac)

lexExp :: ReadP (Maybe Integer)
lexExp = do char 'e' +++ char 'E'
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

valDig 10 c = valDecDig c

valDig 16 c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | 'a' <= c && c <= 'f' = Just (ord c - ord 'a' + 10)
  | 'A' <= c && c <= 'F' = Just (ord c - ord 'A' + 10)
  | otherwise            = Nothing

valDecDig c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
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
