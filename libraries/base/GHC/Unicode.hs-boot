{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Unicode where

import GHC.Types

isAscii         :: Char -> Bool
isLatin1        :: Char -> Bool
isControl       :: Char -> Bool
isPrint         :: Char -> Bool
isSpace         :: Char -> Bool
isUpper         :: Char -> Bool
isLower         :: Char -> Bool
isAlpha         :: Char -> Bool
isDigit         :: Char -> Bool
isOctDigit      :: Char -> Bool
isHexDigit      :: Char -> Bool
isAlphaNum      :: Char -> Bool

