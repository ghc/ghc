module MakeNFA 
  (makeNFA)
where


import OrdList
import FiniteMap
import Matchers
import Assertions
import AbsTreeDefs
import AugTreeDefs
import NFADefs
import ConstructorMonad
import HandleSubexp
import HandleInterval
import MakeNFANode
import BagRE

{- build an NFA from the augmented abstract syntax tree, given an
   initial nfa
   (contains follow info, interval info, and subexp info)
-}

--makeNFA :: AugTree a -> NFA a -> (NFA a,Int) 
makeNFA t nfa = 
        case (initLbl (foldPostM (handleNode (followInfoNoInt t)) nfa t) 1) of
          (nfa,x) -> (addFollowInfo t nfa,x)

-- Add subexp and interval info for nfa

handleNode :: [(Int,BagRE Int)] -> AugTree a -> NFA a -> LblM (NFA a)

handleNode fllws t nfa = 
                   returnLbl (makeNode t nfa) `thenLbl` \newnfa ->
                   (if isInterval t then
                      handleInterval t fllws newnfa
                    else
                      returnLbl (newnfa)) `thenLbl` \newnfa1 ->
                   if isSub t then
                     returnLbl (handleSubexp t newnfa1)
                   else
                     returnLbl (newnfa1)



