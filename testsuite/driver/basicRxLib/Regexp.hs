module Regexp 
   (MatcherFlag(..),
    Assert,
    Matcher,
    REmatch,
    legalRegexp,
    matchedAny,
    numSubexps,
    subexpMatch,
    matchedSubexp,
    allSubexps,
    wholeMatch,
    beforeMatch,
    afterMatch,
    searchP,
    searchS,
    searchExtP,searchBasicP,
    searchExtS,
    substP,
    substS,
    substExtP,substExtS)

where

import Matchers(MatcherFlag(..),Matcher)
import Assertions(Assert)
import NFADefs(NFANode(..),NFAElem,IntervalInfo,SubexpInfo)
import ExecRE(execRegexp)
import CompileRES(compileRegexpS)
import CompileREP(compileRegexpP)
import CompileREPB(compileRegexpPB)
import FiniteMap




-- The search function returns an REmatch. The result from this can be
-- accessed with the functions below.

data REmatch a 
   =  ILLEGAL
   |  NOTHING    -- Match failed
   |  JUST  (FiniteMap String [a], -- matched subexpressions ($1..$n)
             [a],		-- everything before match ($` in perl)
             [a],		-- entire match            ($& in perl)
             [a]		-- everything after match  ($' in perl)
            )              
      

matchedAny :: (REmatch a) -- given match result
              -> Bool	  -- whether there was a match

matchedAny (JUST (_,_,_,_)) = True
matchedAny NOTHING = False  
matchedAny ILLEGAL = False

legalRegexp :: (REmatch a) -> Bool
legalRegexp ILLEGAL = False
legalRegexp _ = True

numSubexps :: (REmatch a)	-- given match result 
           -> Int		-- how many subexpressions matched
numSubexps (JUST (subexps,_,_,_)) =
                length (fmToList subexps)

numSubexps (NOTHING) = 0
numSubexps ILLEGAL = 0

matchedSubexp :: (REmatch a) -> String -> Bool
matchedSubexp (JUST (subexps,_,_,_)) x  = case lookupFM subexps x of 
                                             Nothing -> False 
                                             (Just _) -> True
matchedSubexp (NOTHING) x = False
matchedSubexp ILLEGAL x = False
 
subexpMatch :: (REmatch a)	-- given match result
                -> String	-- the subexp we're interested in
				-- (referred to by number, the first is 1...)
                -> [a]          -- what the subexp matched
subexpMatch rem@(JUST (subexps,_,_,_)) x = 
                case lookupFM subexps x of 
                     Nothing -> []
                     (Just xs) -> xs 
subexpMatch NOTHING x = []   
subexpMatch ILLEGAL x = []

allSubexps :: REmatch a -> [(String,[a])] 
allSubexps (JUST (subexps,_,_,_)) = fmToList subexps
allSubexps NOTHING = []   
allSubexps ILLEGAL = []

wholeMatch :: (REmatch a)	-- given match result
              -> [a]		-- the entire match
wholeMatch (JUST (_,_,piece,_)) = piece
wholeMatch NOTHING = [] 
wholeMatch ILLEGAL = []

beforeMatch :: (REmatch a)	-- given match result
               -> [a]		-- everything before the match
beforeMatch (JUST (_,before,_,_)) = before
beforeMatch NOTHING = [] 
beforeMatch ILLEGAL = []

afterMatch :: (REmatch a)	-- given match result
              -> [a]		-- everything after the match
afterMatch (JUST (_,_,_,after)) = after
afterMatch NOTHING = [] 
afterMatch ILLEGAL = []


-- The polymorphic search function, it compiles the regexp and then performs
-- the match. It finds the first match, of longest(isH) length.
-- It returns an REmatch, defined above.
searchP :: (Eq a, Ord a, Enum a, Read a, Show a) =>
             String		-- regexp
          -> [a]		-- list to match along
          -> (REmatch a)	-- what matched and where
searchP re inp = searchExtP re [] [] inp

searchExtP :: (Eq a, Ord a, Enum a, Read a, Show a) =>
                String		-- regexp
	     -> [Matcher a]     -- pass in own match functions
             -> [Assert a]      -- pass in own assertions
             -> [a]		-- list to match along
             -> (REmatch a)	-- what matched and where
searchExtP re fs as inp 
 = case (compileRegexpP re [] fs as) of 
    (Just matcher) -> 
      case (execRegexp matcher inp) of
        Nothing -> NOTHING
        Just stuff -> JUST stuff
    Nothing -> ILLEGAL         


-- a polymorphic regexp without the need for enum ...

searchBasicP :: (Eq a, Read a, Show a) =>
                String		-- regexp
	     -> [Matcher a]     -- pass in own match functions
             -> [Assert a]      -- pass in own assertions
             -> [a]		-- list to match along
             -> (REmatch a)	-- what matched and where
searchBasicP re fs as inp 
 = case (compileRegexpPB re [] fs as) of 
    (Just matcher) -> 
      case (execRegexp matcher inp) of
        Nothing -> NOTHING
        Just stuff -> JUST stuff
    Nothing -> ILLEGAL         

-- The string search function, it compiles the regexp and then performs the
-- match. It finds the first match, of longest(isH) length.
-- It returns an REmatch, defined above.

searchS :: 
             String		-- regexp
	  -> [MatcherFlag]	-- flags to modify match
          -> String		-- list to match along
          -> (REmatch Char)	-- what matched and where
 
searchS re flags inp = searchExtS re flags [] [] inp

searchExtS :: 
                String		-- regexp
	     -> [MatcherFlag]	-- flags to modify match
	     -> [Matcher Char]-- pass in own match functions
             -> [Assert Char]      -- pass in own assertions
             -> String		-- list to match along
             -> (REmatch Char)	-- what matched and where

searchExtS re flags fs as inp
 = case (compileRegexpS re flags fs as) of 
    (Just matcher) -> 
      case (execRegexp matcher inp) of
        Nothing -> NOTHING
        Just stuff -> JUST stuff
    Nothing -> ILLEGAL         

resultsS re flags fs as inp = case (searchExtS re flags fs as inp) of
                            NOTHING -> Nothing
                            JUST (ses,a,b,c) -> Just (eltsFM ses,a,b,c)




-- search and replace functions.

substP :: (Ord a,Enum a,Read a, Show a) => 
          String	-- regexp
       -> String	-- what to replace it with
       -> [MatcherFlag]	-- flags to modify match
       -> [a]		-- list to match along
       -> [a]		-- result
substP rexp repl flags inp 
  = substExtP rexp repl flags [] [] inp


substS :: String	-- regexp
       -> String	-- what to replace it with
       -> [MatcherFlag]	-- flags to modify match
       -> String	-- list to match along
       -> String	-- result
substS rexp repl flags inp
  = substExtS rexp repl flags []  [] inp


substExtS :: String		-- regexp
          -> String		-- what to replace it with
          -> [MatcherFlag]	-- flags to modify match
          -> [Matcher Char]	-- pass in own match functions
          -> [Assert Char]	-- pass in own assertions
          -> String		-- list to match along
	  -> String		-- result
substExtS rexp repl flags fs as inp 
   =  find inp
   where
    global = Global_Match `elem` flags
    searcher= searchExtS rexp flags fs as
    replacer = replaceS repl
    find sub
     = let
       search_res = searcher sub
       success = matchedAny search_res
       in
         if not success then
           sub
         else          
           let
             match = wholeMatch search_res
             prefix = beforeMatch search_res
             afterthis = afterMatch search_res
             suffix 
              = if global && ((not.null) match) then
                  find afterthis
               else
                  afterthis
           in
               concat [prefix,
                       replacer search_res,
                       suffix]
 

{-
substExtP
      - does a search and replace using Input regexp and replace info
        of perl like format, single elements still in curly brackets.
        Allowed MatcherFlag is Global_Match for doing global replacement;
        otherwise only replaces first occurrence
-}

substExtP :: 
         (Ord a,Enum a,Read a, Show a) =>
         String		-- regexp
      -> String		-- what to replace it with
      -> [MatcherFlag]	-- flags to modify match
      -> [Matcher a]	-- pass in own match functions
      -> [Assert a]	-- pass in own assertions
      -> [a]		-- list to match along
      -> [a]		-- result

substExtP rexp repl flags as fs inp
   =  find inp
   where
    global = Global_Match `elem` flags
    searcher= searchExtP rexp as fs
    replacer = replaceP repl
    find sub
     = let
       search_res = searcher sub
       success = matchedAny search_res
       in
         if not success then
           sub
         else          
           let
             match = wholeMatch search_res
             prefix = beforeMatch search_res
             afterthis = afterMatch search_res
             suffix 
              = if global && ((not.null) match) then
                  find afterthis
               else
                  afterthis
           in
               concat [prefix,
                       replacer search_res,
                       suffix]
 


---------------------------------------------------------
{-
replaceP - uses the REmatch given, and a string of replacement info to
           build the replacement list.
           eg if working on type Days of Week, and ${m} = [Monday,Tuesday]
              replace rem "${m}<Sunday>" = [Monday,Tuesday,Sunday]
-}
replaceP :: (Read a, Show a) =>
            String
         -> REmatch a 
         -> [a] 

replaceP replacement rem = replace' replacement rem
   where
     replacer = searchS "\\< elem@(([^\\\\>]|\\.)+)\\>|\\$" []
     replace' repl rem =
        let matchres = replacer repl
        in
          if not (matchedAny matchres) then
             []
          else if matchedSubexp matchres "elem" then
             [(read (subexpMatch matchres "elem"))] ++
             replace' (afterMatch matchres) rem
          else
             case handleReference rem (afterMatch matchres) of
               Nothing -> replace' (afterMatch matchres) rem
               (Just (ms,as)) -> ms ++ replace' as rem   
 

ref = searchS "^\\{( bef@(m_before_)| aft@(m_after_)| whole@(m)| sub@(\\w+))\\}" []

handleReference :: (Read a, Show a) => 
                   REmatch a -> String -> Maybe ([a],[Char]) 
handleReference rem repl =
       let matchres = ref repl
       in 
         if not (matchedAny matchres) then
            Nothing
         else if (matchedSubexp matchres "whole") then -- matched ${m}
            Just (wholeMatch rem, afterMatch matchres)
         else if matchedSubexp matchres "bef" then -- matched ${m_before_}
            Just (beforeMatch rem, afterMatch matchres)
         else if matchedSubexp matchres "aft" then -- matched ${m_after_}
            Just (afterMatch rem,afterMatch matchres)
         else --if matchedSubexp matchres "sub" then -- matched ${..}...
            Just (subexpMatch rem (subexpMatch matchres "sub"),
                  afterMatch matchres)

{-
replaceS - uses the REmatch given, and a string of replacement info to
           build the replacement list.
           eg if ${m} = "ab"
              replace "${m}d" rem = "abd"
-}
replaceS replacement rem = replace' replacement rem
  where
     replacer = searchS "\\\\ el@(.)|\\$" [] 
     replace' repl rem =
        let matchres = replacer repl
        in
          if not (matchedAny matchres) then
             repl
          else if matchedSubexp matchres "el" then
             beforeMatch matchres ++ subexpMatch matchres "el" 
             ++ replace' (afterMatch matchres) rem
          else
             beforeMatch matchres ++ 
             (case handleReference rem (afterMatch matchres) of
               Nothing -> replace' (afterMatch matchres) rem
               (Just (ms,as)) -> ms ++ replace' as rem) 
     
             

 




