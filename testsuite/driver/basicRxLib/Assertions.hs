module Assertions 
        (Assert,
         doAssert,
         prefix,
	 suffix,
	 bol,
	 eol,
	 wordBound,
	 notWordBound)

where

import Matchers

-- a lookahead/lookbehind assertion to be applied to matcher 
type Assert a =        [a]	-- before this position (look behind)
                    -> [a]	-- after this position (look ahead)
                    -> Bool	-- whether assertion about these holds



doAssert :: Assert a         -- assertions that must be tried
         -> MatcherImpl a        -- The matcher

doAssert assertion bef as
           = if assertion bef as then
               Just ([],as)
             else
              Nothing

-- make the given matcher accept only prefixes
prefix :: Assert a 
prefix [] _ = True
prefix (x:xs) _ = False

-- make the given matcher accept only suffixes
suffix :: Assert a
suffix _ [] = True
suffix _ (x:xs) = False 

-- make an assertion to allow matching from only beginning of lines
bol:: Assert Char
bol [] _ = True
bol ('\n':xs) _ = True
bol _ _ = False

-- make an assertion to allow matching at only end of lines
eol:: Assert Char
eol _ [] = True
eol _ ('\n':xs) = True
eol _ _ = False

wordBound:: Assert Char
wordBound = wbound True

notWordBound:: Assert Char
notWordBound = wbound False

-- make an assertion about word boundaries.
-- will pass if for two adjoining characters one is a \w & one is a \W

wbound     :: Bool    -- whether this is a positive assertion (want wboundary) 
           -> Assert Char	-- the resulting assertion
wbound _ [] [] = False

wbound isWbound [] (m:_) = (isWordChar m) ==  isWbound

-- before info is reversed!!!
wbound isWbound (b:_) [] = (isWordChar b) == isWbound

	            -- exclusive or combination, as
	            -- either want to have a \w & be looking for \W
	            -- or want to have a \W & be looking for \w
wbound isWbound (b:_) (m:_) = 
                          ((isWordChar b && not (isWordChar m))
                          || (not (isWordChar b) && (isWordChar m)))  
                          == isWbound






