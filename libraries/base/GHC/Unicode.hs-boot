{-# OPTIONS -fno-implicit-prelude #-}

module GHC.Unicode where
import GHC.Base( Char, Bool )

isAscii		:: Char -> Bool
isLatin1	:: Char -> Bool
isControl	:: Char -> Bool
isPrint		:: Char -> Bool
isSpace		:: Char -> Bool
isUpper		:: Char -> Bool
isLower		:: Char -> Bool
isAlpha		:: Char -> Bool
isDigit		:: Char -> Bool
isOctDigit	:: Char -> Bool
isHexDigit	:: Char -> Bool
isAlphaNum	:: Char -> Bool
