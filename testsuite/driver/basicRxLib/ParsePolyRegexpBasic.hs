module ParsePolyRegexpBasic 
  (mkAbstreePB)
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

mkAbstreePB :: (Read a, Show a, Eq a) => 
                String			-- The regexp in string form 
             -> [Matcher a]		-- Extra matcher functions
             -> [Assert a]		-- extra assertions
             -> Maybe (AbsTree a b)    	-- An abstract syntax tree
              
mkAbstreePB [] fs as = Just mkEnd
mkAbstreePB re fs as
     = let 
         matchresults = initParser nnRegexpPB re fs as []
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
                | "[" ["^"] Element +"]". 
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

nnElemPB :: (Read a, Show a) => (Parser Char a (Matcher a) (Assert a)) 
nnElemPB = lit '<' ..+ star (butC ['\\','>'] ||| (lit '\\' ..+ anyC)) +.. lit '>' <<< read 

-- parse an atom
nnAtomPB :: (Read a, Show a, Eq a) 
           => (Parser Char (AbsTree a b) (Matcher a) (Assert a))
nnAtomPB 
  =
       (lit '<' ..+ lit '<' ..+ anyPos +.. lit '>' +.. lit '>') 
          `then_Px` (\x -> lookupFn x)
          <<< mkEl.wrapMatcher
    |||  lit '.' <<< const (mkEl matchAny)
    |||  lit '[' ..+ opt (lit '^') 
           +.+ (plus nnElemPB <<< handleelems)
         +.. lit ']' <<< mkEl.matchEls.handleclass
    |||  nnElemPB  <<< mkEl.matchEls.isEl

      where 

	handleelems es = (`elem` es)

	-- deal with a class
	handleclass  ([],g) = g
	handleclass  ([f],g) = (not.g)



-- parse an extra atom
nnXAtomPB :: (Read a, Show a, Eq a) 
            => (Parser Char (AbsTree a b) (Matcher a) (Assert a))

nnXAtomPB =

         (lit '(' ..+ nnRegexpPB +.. lit ')'
      ||| (lit ' ' ..+ (plus (satisfy isWordChar)) +.. lit '@') +.+ lit '(' ..+ nnRegexpPB +.. lit ')' <<< handlesub
      ||| lit '$' ..+ lit '{' ..+ (plus (satisfy isWordChar)) +.. lit '}' 
          <<< (\x -> mkBackref x matchBref)
      ||| nnAtomPB)
      where
        handlesub (s,n) = mkSub s n


     
-- parse an extended atom
nnExtAtomPB :: (Read a, Show a, Eq a) 
             => (Parser Char (AbsTree a b) (Matcher a) (Assert a))

nnExtAtomPB  =  (star 
              ((lit '(' ..+ lit '?' ..+ lit '=' ..+ anyPos +.. lit ')')
               `then_Px` (\x -> lookupAs x) <<< doAssert) <<< combineAssert)
          +.+ ((nnXAtomPB  +.+ opt ((lit '*' <<< const mkStar 
                              ||| lit '+' <<< const mkPlus
                              ||| lit '?' <<< const mkOpt
                              ||| lit '{' ..+ anyPos +.. lit '}'
                                   <<< helpexact
                              ||| lit '{' ..+ anyInt +.+    
                                   lit ',' +.+ (opt anyInt) 
                                   +.. lit '}'       <<< helpminmax 
                                 ) +.+ opt (lit '?') )
                    <<< helprepeat)
          ||| plus (nnAtomPB  `notFollowedBy` oneofC "*+?{") <<< combine
          ||| nnXAtomPB)
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
nnFactorPB :: (Read a, Show a, Eq a) 
            => (Parser Char (AbsTree a b)(Matcher a) (Assert a) )

nnFactorPB = (opt (lit '^')) 
           +.+ (plus nnExtAtomPB) 
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
nnRegexpPB :: (Read a, Show a, Eq a) 
            => (Parser Char (AbsTree a b) (Matcher a) (Assert a))

nnRegexpPB = nnFactorPB +.+ star (lit '|' ..+ nnFactorPB) <<< helper
               where
                 helper (ea,[]) = ea
                 helper (ea, s@(x:xs))= foldl mkAlt ea s



