module CompileREP where


import OrdList

import FiniteMap
import AbsTreeDefs
import AugTreeDefs
import Assertions(Assert)
import Matchers(Matcher,MatcherFlag)
import NFADefs
import ParsePolyRegexp(mkAbstreeP)
import IsPrefix(isPrefix)
import ConstructorMonad(initLbl)
import AugmentTree(augmentT)
import MakeNFA(makeNFA)


compileRegexpP :: (Ord a,Enum a,Read a,Show a) =>
                     String	-- AbsTree in string format
                  -> [MatcherFlag]-- matcher flags
                  -> [Matcher a]-- Extra matcher functions 
                  -> [Assert a] -- extra assertions
                  -> Maybe 
                      (NFA a,	-- The Resulting NFA
                       Int,	-- The last node of the NFA
                       [Node],	-- The initial nodes of the NFA
                       Int,	-- The number of intervals in the AbsTree
                       Bool)	-- Whether we are just matching a prefix


compileRegexpP re flags fs as
    = let abstree1 = (mkAbstreeP re fs as)
          abstree = case abstree1 of
                     (Just a) -> a
          prefix = isPrefix abstree
          (augtree,numnodes) = augmentT abstree
          nfa1 = initNFA numnodes
          (nfa2,numintervals) = makeNFA augtree nfa1
          initnodes = firstPos augtree
      in 
         case abstree1 of
          Just a ->
            Just (nfa2,numnodes,initnodes,numintervals,False)
          Nothing -> Nothing
