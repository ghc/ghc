module HandleSubexp
    (handleSubexp)
where

{- This module provides functions to add subexp info to the nfa.
-}



import FiniteMap
import OrdList
import AbsTreeDefs
import AugTreeDefs
import NFADefs



{- handleSubexp - 
	add information about a subexp node, assumes it has been
	given a subexp node.
-}

handleSubexp :: AugTree a -> NFA a -> NFA a
handleSubexp t nfa
              = dealSubexp (getSEName t) t nfa


{- dealSubexp i t nfa 
	Update an nfa with info about subexpression labelled i. The node
        contains a few bits of info:

        firsPos - all the nodes that can start the subexpression
	lastPos - all the nodes that can end the subexpression
	children - all the leaf nodes in the subexpression
	(child t) - the immediate child node of the subexpression.

        When we enter a subexpression, we want to start recording what
	the subexp matched, with mkSubAdd i. We do this for all the child
	nodes of the subexp

	We want to close the subexp when we leave it. This happens
	on any of the last nodes in the subexp. (lastPos n)

	We don't want to end the subexp prematurely. We therefore use
	NFASubContinue ns. This gives all the follownodes of a
	particular lastpos node. These are calculated simply using the
	subexpression, not operators outside of it. So
	with (a+|b+)*, "a" will be a follownode of "a", and "b" of "b", 
	but "b" will not follow "a", and vice versa, as the outer STAR
	operator will not be included. This therefore assumes that the
	NFA has only follow information for the regexp rooted at the
	current subexpression. 

-}		

dealSubexp :: String		-- the label for the new node
             -> AugTree a	-- the root of tree is node to update with
             -> NFA a		-- the nfa to update
             -> NFA a

dealSubexp i t nfa =
     let  
       fs = firstPos t
       chs = children t
       ls = lastPos t
       new1nfa = 
           foldS (updateNode (mkSubAdd i)) nfa chs
		-- start or continue a subexp node but add info about	
		-- what matched here.


       new2nfa = 
           foldr mkSubContinue new1nfa (follows ls t)
		-- a subexp should not be stopped prematurely, try
		-- stopping it later first. We continue with the
		-- subexp, moving to any follow nodes of that lastpos
		-- node first, before moving on.
		-- This allows things like it@(a+), to work, as the
		-- inner repeatable operations will be allowed to
		-- work. As we're only allowing operators insidee the 
		-- subexpression to work, we don't have a problem with
		-- the likes of (a+|b+)*: "a" is not a follownode of b,
		-- and vice versa.

     in
           foldS (updateNode (mkSubend i)) new2nfa ls
			

mkSubAdd      :: String	   -- the label for the subexp
              -> NFANode a -- the node (of the nfa) to be updated
              -> NFANode a -- the node with new substart info in it
        
mkSubAdd i node 
               = changeSES ((NFASubAdd i):(getSES node)) node 
             
mkSubend ::      String	   -- the label for the subexp
              -> NFANode a -- the node (of the nfa) to be updated
              -> NFANode a -- the node with new subend info in it

mkSubend i node 
             = changeSES (getSES node ++ [NFASubEnd i]) node
       


mkSubContinue :: (Node,[Node])	-- the node to modify & all follow nodes of it
              -> NFA a		-- the nfa to update
              -> NFA a		-- the updated nfa

mkSubContinue (nodeLbl,nexts) nfa = 
	  updateNode (\node -> (changeSES (getSES node ++ [NFASubContinue nexts]) node))
                     nodeLbl nfa






