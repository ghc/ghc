#ifdef HEAD
module UnicodePrims 
	( primUnicodeIsPrint
	, primUnicodeIsUpper
	, primUnicodeIsLower
	, primUnicodeIsAlphaNum
	) where

import PreludeBuiltin
#endif /* HEAD */
#ifdef BODY

-- based on GHC's implementation
primUnicodeIsPrint    c = not (isControl c)
-- The upper case ISO characters have the multiplication sign dumped
-- randomly in the middle of the range.  Go figure.
primUnicodeIsUpper c	=  c >= 'A' && c <= 'Z' || 
                           c >= '\xC0' && c <= '\xD6' ||
                           c >= '\xD8' && c <= '\xDE'
-- The lower case ISO characters have the division sign dumped
-- randomly in the middle of the range.  Go figure.
primUnicodeIsLower c	=  c >= 'a' && c <= 'z' ||
                           c >= '\xDF' && c <= '\xF6' ||
                           c >= '\xF8' && c <= '\xFF'
primUnicodeIsAlphaNum c = isAlpha c  ||  isDigit c
primUnicodeToUpper    c 
          | isLower c   = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
	  | otherwise   = c
primUnicodeToLower    c 
          | isUpper c   = toEnum (fromEnum c - fromEnum 'A' + fromEnum 'a')
	  | otherwise   = c

#endif /* BODY */
