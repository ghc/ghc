-----------------------------------------------------------------------------
-- Standard Library: Char operations
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module Char ( 
    isAscii, isLatin1, isControl, isPrint, isSpace, isUpper, isLower,
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
    digitToInt, intToDigit,
    toUpper, toLower,
    ord, chr,
    readLitChar, showLitChar, lexLitChar,

    -- ... and what the prelude exports
    Char, String
    ) where

-- This module is (almost) empty; Char operations are currently defined in
-- the prelude, but should eventually be moved to this library file instead.
-- No Unicode support yet. 

isLatin1 c = True

-----------------------------------------------------------------------------
