module Char ( 
    isAscii, isControl, isPrint, isSpace, isUpper, isLower,
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphanum, toUpper, toLower ) where

isAscii, isControl, isPrint, isSpace, isUpper,
 isLower, isAlpha, isDigit, isOctDigit, isHexDigit, isAlphanum :: Char -> Bool
isAscii c	 	=  fromEnum c < 128
isControl c		=  c < ' ' || c >= '\DEL' && c <= '\x9f'
isPrint c		=  not (isControl c)
-- This includes non-breaking space
isSpace c		=  c `elem` " \t\n\r\f\v\xa0"
-- The upper case ISO characters have the multiplication sign dumped
-- randomly in the middle of the range.  Go figure.
isUpper c		=  c >= 'A' && c <= 'Z' || 
                           c >= '\xC0' && c <= '\xD6' ||
                           c >= '\xD8' && c <= '\xDE'
-- The lower case ISO characters have the division sign dumped
-- randomly in the middle of the range.  Go figure.
isLower c		=  c >= 'a' && c <= 'z' ||
                           c >= '\xDF' && c <= '\xF6' ||
                           c >= '\xF8' && c <= '\xFF'
isAlpha c		=  isUpper c || isLower c
isDigit c		=  c >= '0' && c <= '9'
isOctDigit c		=  c >= '0' && c <= '7'
isHexDigit c		=  isDigit c || c >= 'A' && c <= 'F' ||
                                        c >= 'a' && c <= 'f'
isAlphanum c		=  isAlpha c || isDigit c

-- These almost work for ISO-Latin-1 (except for =DF <-> =FF)

toUpper, toLower	:: Char -> Char
toUpper c | isLower c	=  toEnum (fromEnum c - fromEnum 'a'
                                              + fromEnum 'A')
	  | otherwise	=  c

toLower c | isUpper c	=  toEnum (fromEnum c - fromEnum 'A' 
                                              + fromEnum 'a')
	  | otherwise	=  c
