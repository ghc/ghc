module ParsePolyRegexp 
  (mkAbstreeP)
where

{-
This module provides functions to parse a polymorphic regexp.
The main one is mkAbstree. It takes a string form of the AbsTree, and
returns an Abstract Syntax tree for it. 
-}

import Matchers
import Assertions
import Parsers
import FiniteMap
import AbsTreeDefs


-- Turn the regular expression concrete syntax into an abstract syntax tree

mkAbstreeP :: (Read a, Show a, Eq a, Ord a, Enum a) => 
                String			-- The regexp in string form 
             -> [Matcher a]		-- Extra matcher functions
             -> [Assert a]		-- extra assertions
             -> Maybe (AbsTree a b)    	-- An abstract syntax tree
              
mkAbstreeP [] fs as = Just mkEnd
mkAbstreeP re fs as
     = let 
         matchresults = initParser nnRegexp re fs as []
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
  Elem          = "<" (character)+ ">"
  Atom          = "." | "<<" Int ">>" | Element.
                | "[" ["^"] (Element + | ( Element "-" Element)+) +"]". 
  XAtom         = "\\" Int | "(" AbsTree ")" | "(?:" AbsTree ")" | Atom .
  ExtAtom       = "(?=" Int ")"
                | (XAtom ["*" | "+" | "?" 
                         | "{" Int "}"
                         | "{" Int "," [Int] "}" 
                         ] ["?"])
                | Atom +
                | XAtom.
  Factor        = "^" ExtAtom + "$".
  AbsTree        = Factor "|" (Factor *).
-}

nnElem :: (Read a,Show a) => (Parser Char a (Matcher a) (Assert a)) 
nnElem = lit '<' ..+ star (butC ['\\','>'] ||| (lit '\\' ..+ anyC)) +.. lit '>' <<< read 

-- parse an atom
nnAtom :: (Read a, Show a, Eq a, Ord a, Enum a) 
           => (Parser Char (AbsTree a b) (Matcher a) (Assert a))
nnAtom 
  =
       (lit '<' ..+ lit '<' ..+ anyPos +.. lit '>' +.. lit '>') 
          `then_Px` (\x -> lookupFn x)
          <<< mkEl.wrapMatcher
    |||  lit '.' <<< const (mkEl matchAny)
    |||  lit '[' ..+ opt (lit '^') 
         +.+ (plus ((plus (nnElem  +.+ lit '-' ..+ nnElem) <<< handlerange)
                ||| (plus nnElem <<< handleelems)))
              +.. lit ']' <<< mkEl.matchEls.handleclass
    |||  nnElem  <<< mkEl.matchEls.isEl

      where 

	handleelems es = (`elem` es)
	handlerange es = foldr1 combine (map inrange es)
	inrange (e1,e2) c = c >=e1 && c<=e2
	combine f g c = (f c) || (g c)

	-- deal with a class
	handleclass  ([],fs) = (foldr1 combine fs)
	handleclass  ([f],fs) = (not.(foldr1 combine fs))



-- parse an extra atom
nnXAtom :: (Read a,Show a, Eq a, Ord a, Enum a) 
            => (Parser Char (AbsTree a b) (Matcher a) (Assert a))

nnXAtom =

         (lit '(' ..+ nnRegexp +.. lit ')'
      ||| (lit ' ' ..+ (plus (satisfy isWordChar)) +.. lit '@') +.+ lit '(' ..+ nnRegexp +.. lit ')' <<< handlesub
      ||| lit '$' ..+ lit '{' ..+ (plus (satisfy isWordChar)) +.. lit '}' 
          <<< (\x -> mkBackref x matchBref)
      ||| nnAtom)
      where
        handlesub (s,n) = mkSub s n


     
-- parse an extended atom
nnExtAtom :: (Read a, Show a, Eq a, Ord a, Enum a) 
             => (Parser Char (AbsTree a b) (Matcher a) (Assert a))

nnExtAtom  =  (star 
              ((lit '(' ..+ lit '?' ..+ lit '=' ..+ anyPos +.. lit ')')
               `then_Px` (\x -> lookupAs x) <<< doAssert) <<< combineAssert)
          +.+ ((nnXAtom  +.+ opt ((lit '*' <<< const mkStar 
                              ||| lit '+' <<< const mkPlus
                              ||| lit '?' <<< const mkOpt
                              ||| lit '{' ..+ anyPos +.. lit '}'
                                   <<< helpexact
                              ||| lit '{' ..+ anyInt +.+    
                                   lit ',' +.+ (opt anyInt) 
                                   +.. lit '}'       <<< helpminmax 
                                 ) +.+ opt (lit '?') )
                    <<< helprepeat)
          ||| plus (nnAtom  `notFollowedBy` oneofC "*+?{") <<< combine
          ||| nnXAtom)
          +.+
              (star
              ((lit '(' ..+ lit '?' ..+ lit '=' ..+ anyPos +.. lit ')')
               `then_Px` (\x -> lookupAs x) <<< doAssert) <<< combineAssert)
          <<< helper
               where
 
                  helpexact x = mkExact x

                  helpminmax (x,(a,[])) = mkMin x
                  helpminmax (x,(a,[y])) 
                         = if x <= y then 
                             mkMinMax x y  
                           else 
                             error "Invalid interval in Regular Expression"

                  helprepeat (ea,[]) = ea
                  helprepeat (ea,[(f,[])]) = f ea
                  helprepeat (ea,[(f,[_])]) = mkNg (f ea)


                  combine = (mkEl . foldl1 regseq . map getElem) 

                  combineAssert [] = []
                  combineAssert s@(x:xs) = [(mkEl.foldl1 regseq) s]
                  helper ([],(t,[])) = t
                  helper ([e1],(t,[])) =  mkCon e1 t
				
                  helper ([e1],(t,[e2])) = mkCon e1 (mkCon t e2)

                  helper ([],(t,[e2])) = mkCon t e2


-- parse a factor
nnFactor :: (Read a, Show a, Eq a, Ord a, Enum a) 
            => (Parser Char (AbsTree a b)(Matcher a) (Assert a) )

nnFactor = (opt (lit '^')) 
           +.+ (plus nnExtAtom) 
           +.+ (opt (lit '$')) 
           <<< helper
      where
         helper ([],(xs,[])) = combine xs	-- not prefix or suffix
         helper ([_],(xs,[])) =  mkCon (justPrefix (mkEl (doAssert prefix))) (combine xs)
						-- is prefix
         helper ([_],(xs,[_])) = mkCon (justPrefix (mkEl (doAssert prefix))) 
                                       (mkCon (combine xs) 
                                              (mkEl (doAssert suffix)))
						-- is prefix & suffix
         helper ([],(xs,[_])) = mkCon (combine xs) (mkEl (doAssert suffix))
         combine (x:[]) = x
         combine s@(x:xs) = foldl1 mkCon s
         

-- parse a regexp
nnRegexp :: (Read a,Show a, Eq a, Ord a, Enum a) 
            => (Parser Char (AbsTree a b) (Matcher a) (Assert a))

nnRegexp = nnFactor +.+ star (lit '|' ..+ nnFactor) <<< helper
               where
                 helper (ea,[]) = ea
                 helper (ea, s@(x:xs))= foldl mkAlt ea s



