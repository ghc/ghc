module MakeNFANode
  (makeNode,
   addFollowInfo)
where


import OrdList
import FiniteMap
import Matchers
import Assertions
import AbsTreeDefs
import AugTreeDefs
import NFADefs

{- build an Nfa node from the augmented abstract syntax tree node, given an
   initial nfa node.
   (contains no follow info, or interval info, or subexp info)
-}

makeNode :: AugTree a -> NFA a -> NFA a
makeNode t nfa = case (getLbl t) of
                   (Just x) -> updateNode (mkNfanode x t) x nfa
                   (Nothing) -> nfa

mkNfanode :: Int -> AugTree a -> NFANode a -> NFANode a
mkNfanode n re cnode
                | isElem re =
                    let  f = getElem re
                     in  mkNfaEl f cnode

		| isBref re =
                    let (x,f) = getBref re
                    in
                        mkNfaBack x f cnode

		| otherwise = mkNfaFinal n cnode

{- Add info about what follows each node, to the nfa -}

addFollowInfo :: AugTree a -> NFA a -> NFA a
addFollowInfo t nfa = foldr addtoNFA nfa (followInfo t)
  where
   addtoNFA (x,nexts) nfa = updateNode (changeNexts nexts) x nfa

