module Matchers 
	(MatcherFlag(..),
         initFlags,
         MatcherImpl,
         Matcher,
         wrapMatcher,
         regseq,
         isEl,
	 isWordChar,
	 matchEls,
	 matchAny,
	 matchAnyBut,
         matchNull,
	 matchBref,
	 matchIBref)
	 
where

import Char
import FiniteMap

data MatcherFlag = Global_Match     -- replace every occurrence of match
                                   -- rather than just the first in "subst"
                 | Case_Insensitive -- ignore case when using strings
                 | Case_Sensitive   -- don't ignore case when using strings 
                 | Single_Line     -- treat string as a single line
                                   -- . matches \n
                 | Multi_Line      -- treat string as multi-line 
				   -- (^ ($) matches from beginning (end) of line)
		 | Default_Line    -- . does not match \n, but 
				   -- (^ does not match from beginning of line...)

                 deriving (Read,Show,Eq)

{-
instance Read MatcherFlag where
 readsPrec 0 "Global_Match" = [(Global_Match,"")]
 readsPrec 0 "Case_Insensitive" = [(Case_Insensitive,"")]
 readsPrec 0 "Case_Sensitive" = [(Case_Sensitive,"")]
 readsPrec 0 "Single_Line" = [(Single_Line,"")]
 readsPrec 0 "Multi_Line" = [(Multi_Line,"")]
 readsPrec 0 "Default_Line" = [(Default_Line,"")]
-}

initFlags :: [MatcherFlag] -> [(String,MatcherFlag)]
initFlags mflgs = let 
                     casere = if Case_Insensitive `elem` mflgs then
                               Case_Insensitive
                              else 
                               Case_Sensitive
                     linere = if Multi_Line `elem` mflgs then
                               Multi_Line
                              else if Single_Line `elem` mflgs then
                               Single_Line
                              else 
                               Default_Line
                  in
                     [("case",casere),("line",linere)]                    

type Matcher a =  [a]		-- what to match against
               -> Maybe ([a],	-- what matched
                         [a])	-- everything after match

wrapMatcher :: Matcher a -> MatcherImpl a
wrapMatcher f bef inp = f inp

type MatcherImpl a =  [a]	 -- everything before start of this match
			         -- (allows lookbehind assertions)
                   -> [a]	 -- what to match against
                   -> Maybe ([a],-- what matched
                             [a])-- everything after match

regseq ::  (MatcherImpl a) -- a matcher function
     -> (MatcherImpl a) -- the next matcher function
     -> (MatcherImpl a) -- both functions sequenced so
		    -- one follows the other
regseq f g bef af
    = case (f bef af) of 
         Nothing -> Nothing
         Just (ms,as) ->
            case (g ((reverse ms)++bef) as) of
               Nothing -> Nothing
               Just (ms1,as1) -> Just (ms++ms1,as1)

--isEl takes an element and returns a matching function for it.
isEl :: Eq a => a -> a -> Bool
isEl c = (c ==)

-- match a \w 
isWordChar c = isAlphaNum c || (isEl '_') c || (isEl '\'') c



-- create a matcher that accepts an element if matched by f
matchEls :: Eq a => (a -> Bool) -> MatcherImpl a
matchEls f _ [] = Nothing
matchEls f _ (a:as) = if f a then 
                        Just ([a],as)
                      else
                        Nothing

-- create a matcher that matches 0 elements of any non-empty list,  
matchNull :: MatcherImpl a
matchNull _ [] = Nothing 
matchNull _ (x:xs) = Just ([],x:xs)

-- create a matcher that matches any element
matchAny :: MatcherImpl a
matchAny _ [] = Nothing
matchAny _ (a:as) = Just ([a],as)


-- create a matcher that matches any element except '\n'
matchAnyBut :: MatcherImpl Char
matchAnyBut _ [] = Nothing
matchAnyBut _ (a:as) = if a /= '\n' then
                      Just ([a],as)
                    else
                      Nothing

-- create a backreference matcher
matchBref :: Eq a => [a]	-- what to try and match
		    -> MatcherImpl a -- a matcher function	 

matchBref [] _ inps = Just ([],inps)
matchBref (x:xs) _ [] = Nothing
matchBref (x:xs) _ (i:inps) = if x==i then 
                                  case (matchBref xs [] inps) of 
                                    Nothing -> Nothing
                                    Just (ms,afts) -> Just (i:ms,afts)
                                else
                                    Nothing

-- create a backreference matcher, that is case insensitive (for strings)
matchIBref ::  String	 -- what to try and match
	    -> MatcherImpl Char -- a matcher function	 

matchIBref [] _ inps = Just ([],inps)
matchIBref (x:xs) _ [] = Nothing
matchIBref (x:xs) _ (i:inps) = if (toUpper x) == (toUpper i) then 
                                  case (matchIBref xs [] inps) of 
                                    Nothing -> Nothing
                                    Just (ms,afts) -> Just (i:ms,afts)
                                else
                                    Nothing

