module CompileRES where


import FiniteMap   
import AbsTreeDefs
import AugTreeDefs(firstPos,Aug)
import Assertions(Assert)
import Matchers(Matcher,MatcherFlag)
import NFADefs(Node,NFA,NFANode,initNFA) 
import ParseStringRegexp(mkAbstreeS)
import IsPrefix(isPrefix)
import ConstructorMonad(initLbl)
import AugmentTree(augmentT)
import MakeNFA(makeNFA)


compileRegexpS :: 
                     String	-- AbsTree in string format
		  -> [MatcherFlag]-- matcher flags
                  -> [Matcher Char]-- Extra matcher functions 
                  -> [Assert Char] -- extra assertions
                  -> Maybe
                      (NFA Char,-- The Resulting NFA
                       Int,	-- The last node of the NFA
                       [Node],	-- The initial nodes of the NFA
                       Int,	-- The number of intervals in the AbsTree
                       Bool)	-- Whether we are just matching a prefix


compileRegexpS re flags fs as 
    = let (abstree0) = (mkAbstreeS re fs as flags)
          abstree = case abstree0 of
                      (Just a) -> a
          prefix = isPrefix abstree
          (augtree,numnodes) = augmentT abstree
          nfa1 = initNFA numnodes
          (nfa2,numintervals) = makeNFA augtree nfa1
          initnodes = firstPos augtree
      in 
         case abstree0 of 
          Just a ->
           Just (nfa2,numnodes,initnodes,numintervals,prefix)
          Nothing -> Nothing




makeit re = 
 let      (abstree0) = (mkAbstreeS re [] [] [])
          abstree = case abstree0 of
                      (Just a) -> a
          prefix = isPrefix abstree
          (augtree,numnodes) = augmentT abstree
          nfa1 = initNFA numnodes
          (nfa2,numintervals) = makeNFA augtree nfa1
          initnodes = firstPos augtree
      in 
         augtree
