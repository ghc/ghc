module ParseStringRegexp 
  (mkAbstreeS)
where

{-
This module provides functions to parse a string regexp.
The main one is mkAbstree. It takes a string form of the AbsTree, and
returns an Abstract Syntax tree for it. 
-}


import Matchers
import Assertions
import Parsers
import FiniteMap
import AbsTreeDefs
import Char

-- Turn the regular expression concrete syntax into an abstract syntax tree

mkAbstreeS ::  
                String			-- The regexp in string form 
             -> [Matcher Char]		-- Extra matcher functions
             -> [Assert Char]
             -> [MatcherFlag]		-- The necessary matcher flags
             -> Maybe (AbsTree Char b)	-- An abstract syntax tree
              

mkAbstreeS [] fs as flags = Just mkEnd
mkAbstreeS re fs as flags
     = let 
         matchresults = initParser nnRegexpS re fs as (initFlags flags)
         (match,rest) = head matchresults 
         wellformed = (not (null matchresults)) && (null rest)
         realmatch = addEnd match
       in
         if wellformed then
           Just realmatch
         else
           Nothing

--
-- grammar for regular expressions:
--
{-
  Element       = nonmeta char | "\\" special meta char | "\\" character 
  Atom          = ("(?" (Mode [","]) +")") 
                  ( "." | "<<" Int ">>" | Element
                  | "[" ["^"] (Element + | ( Element "-" Element)+) +"]"
                  )("(?" (Mode [","]) +")"). 
  XAtom         = "\\" Int | "(" AbsTree ")" | "(?:" AbsTree ")" | Atom .
  ExtAtom       = "(?=" Int ")"
                | (XAtom ["*" | "+" | "?" 
                         | "{" Int "}"
                         | "{" Int "," [Int] "}" 
                         ] ["?"])
                | Atom +
                | XAtom.
  Factor        = ("^" | "\\A") ExtAtom + ("$" | "\\Z").
  AbsTree       = Factor "|" (Factor *).
-}


nnElemS :: (Parser Char Char (Matcher Char) (Assert Char))
nnElemS = butC "$.*+?@|()^[]\\" ||| lit '\\' ..+ butC ("wWsSdDbBAZ"++['0'..'9'])

-- match a character class element
nnCCEl::(Parser Char Char (Matcher Char) (Assert Char))
nnCCEl = butC "]\\" ||| lit '\\' ..+ butC ("wWsSdD") 

--get a special meta character
parseMeta :: (Parser Char (Char -> Bool) (Matcher Char) (Assert Char))
parseMeta = 
        lit '\\' ..+ lit 'w' <<< const isWordChar
    ||| lit '\\' ..+ lit 'W' <<< const (not.isWordChar)
    ||| lit '\\' ..+ lit 's' <<< const isSpace
    ||| lit '\\' ..+ lit 'S' <<< const (not.isSpace)
    ||| lit '\\' ..+ lit 'd' <<< const isDigit
    ||| lit '\\' ..+ lit 'D' <<< const (not.isDigit)

-- parse a mode change
parseChange :: (Parser Char ([String]) (Matcher Char) (Assert Char))
parseChange =     (lit '(' ..+ lit '?' 
                   ..+ plus ((isWord "Case_Insensitive" 
                         ||| isWord "Case_Sensitive"
                         ||| isWord "Default_Line"
                         ||| isWord "Multi_Line"
                         ||| isWord "Single_Line") +.. opt (lit ','))
                   +.. lit ')')
                   `thenxP_` \xs -> changemode xs
     where
	changemode xs = foldl1 compose (map mkUpdate xs)
	compose f g = f ..+ g  
	mkUpdate "Case_Insensitive" = (updateEnv "case" Case_Insensitive)  
	mkUpdate "Case_Sensitive" = (updateEnv "case" Case_Sensitive) 
	mkUpdate "Default_Line" = (updateEnv "line" Default_Line) 
	mkUpdate "Multi_Line" = (updateEnv "line" Multi_Line)  
	mkUpdate "Single_Line" = (updateEnv "line" Single_Line) 

-- parse an assertion
parseAssert :: (Parser Char (MatcherImpl Char) (Matcher Char) (Assert Char))
parseAssert = 
             (lit '(' ..+ lit '?' ..+ lit '=' ..+ anyPos +.. lit ')')
             `then_Px` (\x -> lookupAs x <<<  doAssert ) 
         ||| lit '\\' ..+ (lit 'b' ||| lit 'B') <<< handlewbound
    where
      handlewbound 'b' = doAssert wordBound
      handlewbound 'B' = doAssert notWordBound
                    

-- parse an atom
nnAtomS :: (Parser Char (AbsTree Char b)(Matcher Char) (Assert Char))

nnAtomS 
  =
     star parseChange 
    ..+

       (((lit '<' ..+ lit '<' ..+ anyPos +.. lit '>' +.. lit '>') 
          `then_Px` \x -> lookupFn x)
          <<< mkEl.wrapMatcher  

    |||  lookupEnv "line" +.. 
         lit '.' <<< mkEl.handleany

    |||  lookupEnv "case" `then_Px` \x ->  
	(lit '[' ..+ opt (lit '^') 
         +.+ (plus (parseMeta
                ||| (plus (nnCCEl  +.+ lit '-' ..+ nnCCEl) <<< handlerange x)
                ||| (plus nnCCEl <<< handleelems x)))
              +.. lit ']' <<< mkEl.matchEls.handleclass)

    ||| parseMeta <<< mkEl.matchEls

    ||| lookupEnv "case" +.+  
        nnElemS  <<< mkEl.matchEls.handlecase)
    +.. 
     star parseChange

      where 

        handleany Single_Line = matchAny
        handleany _ = matchAnyBut
    
        handlecase (Case_Sensitive,x) = isEl x
        handlecase (Case_Insensitive,x) 
			| isLower x = \y -> isEl x y || isEl (toUpper x) y
			| isUpper x = \y -> isEl x y || isEl (toLower x) y
                        | otherwise = isEl x

	handleelems Case_Sensitive es el = (el `elem` es)
        handleelems Case_Insensitive es el 
                                        = (toUpper el) `elem` (map toUpper es)

	handlerange Case_Sensitive es = foldr1 combine (map inrange es)
	handlerange Case_Insensitive es = foldr1 combine (map inrangeI es)

        inrange (e1,e2) c = c >=e1 && c<=e2
	inrangeI (e1,e2) = doinIrange e1 e2
	combine f g c = (f c) || (g c)

	-- deal with a class
	handleclass  ([],fs) = (foldr1 combine fs)
	handleclass  ([f],fs) = (not.(foldr1 combine fs))

{- 
   handle a case insensitive range efficiently, by working out which
   characters should be covered.
   Upper case letters occur before lower case, with some characters in
   between. This is ascii dependent.
-}
doinIrange :: Char -> Char -> (Char -> Bool)
doinIrange e1 e2   = if (e1 <= 'A' && e2 >= 'z') || (e2 < 'A') || (e1 > 'z') 
                        || (e1 >'Z' && e2 < 'a')   
                     then -- either all letters already covered or none covered
			(\c -> c >=e1 && c<=e2)

		     else if e1 <= 'A' then -- start before before uppers
                       if e2 >= 'a' then    -- end in lower cases
			(\c -> c >= e1 && c<= 'z') -- all alpha have to be covered

		       else if e2 < 'Z' then  -- covered some of
				              -- uppers, so cover equiv lowers
			(\c -> (c >=e1 && c<=e2) 
			    ||  (c >= 'a' && c<= (toLower e2)))

		       else -- all uppers covered and finish before lower
			(\c -> (c >=e1 && c<=e2) || isLower c) 
                          -- so cover all lowers as well

		     else if e2 >= 'z' then -- finish after lowers
		       if e1 <= 'Z' then    -- start in uppers
			(\c -> c >= 'A' && c <= e2)-- all alpha have to be covered

		       else if e1 > 'A' then -- covers some of lowers,
					     -- so cover equiv uppers
			(\c -> (c >=e1 && c<=e2) 
			    || (c >= 'A' && c<= (toUpper e2)))	
		       
		       else -- all lowers covered and finish before upper
			(\c -> (c >=e1 && c<=e2) || isUpper c)--so cover all uppers too

		     else if e1 < 'Z' then -- start in uppers
		       if e2 <= 'Z' then  -- end in uppers, so cover equiv lowers
			 (\c -> (c >=e1 && c<=e2) 
			     || (c >= (toLower e1) && c<= (toLower e2)))

		       else if e2 >= 'a' then -- end in lowers
			if (toLower e1) <= e2 then -- start in uppers
                                           -- between the two all characters
                                           -- have to be covered
			  (\c -> (c>='A' && c<='z'))-- make all alpha to be covered
			else -- some letters covered, so cover equiv
			  (\c -> (c >=e1 && c<=e2) 
			     ||  (c >= 'A' && c <= (toUpper e2))
			     ||  (c >=(toLower e1) && c<= 'z'))

		       else -- start in uppers & end between uppers & lowers
			(\c -> (c >=e1 && c<=e2) -- cover eqiv lowers
			    || (c >=(toLower e1) && c<= 'z'))

		     else if e1 > 'a' then -- start in lowers & end in lowers
			(\c -> (c >=e1 && c<=e2) -- so cover equiv uppers
			    || (c >=(toUpper e1) && c<= (toUpper e2)))

		     else -- start between uppers & lowers & end in lowers
			(\c -> (c >=e1 && c<=e2) -- cover equiv uppers
			   ||  (c >= 'A' && c <= (toUpper e2)))

-- parse an XAtom
nnXAtomS :: (Parser Char (AbsTree Char b)(Matcher Char) (Assert Char))

nnXAtomS =
     star parseChange
      ..+
         (lit '(' ..+ nnRegexpS +.. lit ')'
      ||| startSubexp +.+ lit '(' ..+ nnRegexpS +.. lit ')' <<< handlesub
      ||| lookupEnv "case" +.+  
          (lit '$' ..+ lit '{' ..+ (plus (satisfy isWordChar)) +.. lit '}')
          <<< handlecase   
         )
      +..
     star parseChange 

      where
        handlecase (Case_Sensitive,x) = mkBackref x matchBref 
        handlecase (Case_Insensitive,x) = mkBackref x matchIBref  
        handlesub (s,n) = mkSub s n


startSubexp = lit ' ' ..+ (plus (satisfy isWordChar)) +.. lit '@'

-- parse an extended atom
nnExtAtomS :: (Parser Char (AbsTree Char b)(Matcher Char) (Assert Char))

nnExtAtomS  = star parseChange ..+
              (star parseAssert <<< combineAssert)
          +.+ ((nnXAtomS ||| nnAtomS)
                         +.+    ((lit '*' <<< const mkStar 
                              ||| lit '+' <<< const mkPlus
                              ||| lit '?' <<< const mkOpt
                              ||| lit '{' ..+ anyPos +.. lit '}'
                                   <<< helpexact
                              ||| lit '{' ..+ anyInt +.+    
                                   lit ',' +.+ (opt anyInt) 
                                   +.. lit '}'       <<< helpminmax 
                                 ) +.+ opt (lit '?') ) <<< helprepeat
         ||| nnXAtomS
         ||| plus (nnAtomS `notFollowedBy` (startSubexp <<< const 'x' ||| oneofC "*+?{")) 
             <<< combine
             -- don't want to eat up the name for a subexpression
             -- want to catch the last atom separately as it is to be
             -- made into a repeatable node
         ||| nnAtomS)
         +.+ (star parseAssert <<< combineAssert)
         +.. star parseChange
         <<< helper

               where
 
                  helpexact x = mkExact x

                  helpminmax (x,(a,[])) = mkMin x
                  helpminmax (x,(a,[y])) 
                         = if x <= y then 
                             mkMinMax x y  
                           else 
                             error "Invalid interval in Regular Expression"

                 
                  helprepeat (ea,(f,[])) = f ea
                  helprepeat (ea,(f,[_])) = mkNg (f ea)

                  combine = mkEl . foldl1 regseq . map getElem 

		  combineAssert [] = []
                  combineAssert s@(x:xs) = [(mkEl.foldl1 regseq) s]

                  helper ([],(t,[])) = t
                  helper ([e1],(t,[])) =  mkCon e1 t
				
                  helper ([e1],(t,[e2])) = mkCon e1 (mkCon t e2)

                  helper ([],(t,[e2])) = mkCon t e2


-- parse a factor
nnFactorS :: (Parser Char (AbsTree Char b)(Matcher Char) (Assert Char))

nnFactorS =    star parseChange ..+ 
               (opt (lookupEnv "line" +.+ (lit '^' -- may match begin any line
                                       ||| lit '\\' ..+ lit 'A'))) 
                                                   -- match only begin string
           +.+ (plus nnExtAtomS) 
           +.+ (opt (lookupEnv "line" +.+ (lit '$' -- may match end any line
                                      ||| lit '\\' ..+ lit 'Z'))) 
						  -- match only end line
               +.. star parseChange
           <<< helper

      where
         helper ([],(xs,[])) = combine xs	-- not prefix or suffix
	 helper ([(mode,what)],(xs,[]))
              = handlepre (mode,what) (combine xs)
	 helper ([],(xs,[(mode,what)]))
	      = handlesuff (mode,what) (combine xs) 
         helper ([(mode1,what1)],(xs,[(mode2,what2)])) 
              = handlepre (mode1,what1) (handlesuff (mode2,what2) (combine xs))

	 handlepre (_,'A') t = mkCon (justPrefix (mkEl (doAssert prefix))) t
	 handlepre (Multi_Line,'^') t = mkCon (mkEl (doAssert bol)) t
         handlepre (_,'^') t = mkCon (justPrefix (mkEl (doAssert prefix))) t

	 handlesuff (_,'Z') t = mkCon t (mkEl (doAssert suffix))
	 handlesuff (Multi_Line,'$') t =  mkCon t (mkEl (doAssert eol))
         handlesuff (_,'$') t = mkCon t (mkEl (doAssert suffix))

         combine (x:[]) = x
         combine s@(x:xs) = foldl1 mkCon s
         

-- parse a regexp
nnRegexpS :: (Parser Char (AbsTree Char b)(Matcher Char) (Assert Char))

nnRegexpS = nnFactorS +.+ star (lit '|' ..+ nnFactorS) <<< helper
               where
                 helper (ea,[]) = ea
                 helper (ea, s@(x:xs))= foldl mkAlt ea s

