module HandleInterval
    (handleInterval)
where

{- adds interval info to the nfa, eg a{1,3} stuff 
   augtree is being parsed in post order, so
   nested intervals: child has lower number than parent, 
                     left child lower than right child 
-}

import OrdList
import FiniteMap
import BagRE
import AbsTreeDefs
import AugTreeDefs
import ConstructorMonad
import NFADefs


handleInterval ::  AugTree a	   -- The Info from the augmented tree
               -> [(Int,BagRE Int)]-- the follow info from root of whole tree
               -> NFA a		   -- The current nfa
               -> LblM (NFA a)	   -- The updated nfa
handleInterval t followers nfa = 
    let
     rfollows = filter (\(a,b) -> a `elem` (lastPos t)) followers
     newfollows = map (\(a,b)->bagREToOL (b `minusBagRE` (getFollow a (followInfoNoInt t )))) rfollows 
     getFollow a fs = case (filter ((== a).fst) fs) of
                        [] -> emptyBagRE
                        ((_,x):_) -> x

     nexts = foldr (\ns next -> next `unionOL` ns) emptyOL newfollows
   {- the nodes we go to after leaving the interval, nexts, are all follow
      nodes, not including those produced by operators inside the
      interval. ie with (ab+){2} the nexts  nodes would be [1,3] where
      node 1 is "a", and node 3 is the final node.
   -}
 
    in
          getNewLbl `thenLbl` \name ->
          returnLbl (dealInterval t name nexts nfa)    


dealInterval :: AugTree a -- The Info from the augmented tree
             -> Int	  -- The label for this interval
             -> OrdList Node -- the next nodes that follow after interval
             -> NFA a     -- The current nfa
             -> NFA a     -- The updated nfa

{- For the expression, e,  rooted with this node: -}

{-	MIN x - It starts in any of the first nodes of e
		It ends in any of the last nodes of e,
		Before matching x times, at end of e can go to start of e.
		After matching x times, can go to start of e, or next after e.
		It must be reset in any node following the last nodes
		of e. (nested mins otherwise are never restarted.)
                Interval may end prematurely if the last node of the
                interval is nullable. In this case add IntContinues,
                to make sure we try match through the necessary number
		of times.
                Eg (ab*c?){4} may end with a, b or c,
                we would have [(2,[2,3]),(3,[3])] to allow the *, and
		+ operators to match as many times as possible
                note with (a+|b){2}, that b should not follow a. We	
		don't face this problem as we're using follownodes
		based on what's inside the interval.
                With (a{3,}b){4,}, we don't want Node "a" linking to
		Node "a" (with an IntContinue), as this would 
                interfere with the operation
		of the inner interval. So we wish to use the follow
		nodes without interval information added. 
-}

dealInterval t name nexts nfa | isMin t && not (isNg t)
   = let 
      fs = firstPos t
      chs = children t
      ls = lastPos t
      nfa1 = foldS (updateNode (addIntervalStart name)) nfa fs
      nfa2 = foldr (addIntContinue False) nfa1 (followsWithoutInt ls t) 
      nfa3 = foldS (updateNode (addMinEnd name (getMin t) fs nexts)) nfa2 ls
         in 
                  foldS (updateNode (addReset name)) nfa3 
                               (minusOL (followNodes ls nfa) chs)

{-    NGMIN x - As for MIN above, except that given choice we want to
		move out of interval as soon matched x times. (if possible)
		
-}

dealInterval t name nexts nfa  | isMin t 
       = let 
	   fs = firstPos t
           chs = children t
           ls = lastPos t
           nfa1 = foldS (updateNode (addIntervalStart name)) nfa fs
           nfa2 = foldr (addIntContinue True) nfa1 (followsWithoutInt ls t)
           nfa3 = foldS (updateNode (addNGMinEnd name (getMin t) fs nexts)) nfa2 ls
         in  foldS (updateNode (addReset name)) nfa3
                        (minusOL (followNodes ls nfa) chs)
            
 
{- MINMAX x y - Starts in any of the first nodes of e
		Ends in any of the last nodes of e,
		Before matching x times, at end of e can go to start of e.
		Between x and y times, can go to start of e, or next after e.
		After matching y times, can go to next after e.
                It must be reset in any node following the last nodes
		of e. (nested intervals otherwise are never restarted.)
                Interval may end prematurely if the last node of the
                interval is nullable. We therefore use IntContinues as
		discussed above.
-}
  
   
dealInterval t name nexts nfa |isMinmax t && not (isNg t) 
          = let 
             fs = firstPos t
             chs = children t
             ls = lastPos t
	     (x,y) = getMinMax t
             nfa1 = foldS (updateNode (addIntervalStart name)) nfa fs
             nfa2 =foldr (addIntContinue False) nfa1 (followsWithoutInt ls t)
             nfa3 = foldS (updateNode (addIntEnd name x y fs nexts)) nfa2 ls
            in 
                  foldS (updateNode (addReset name)) nfa3 
                               (minusOL (followNodes ls nfa) chs)

{- NGMINMAX x y - Same as MINMAX except that between x and y times,
		  prefer to go t next after e, than start of e
-}

dealInterval t name nexts nfa | isMinmax t
     = let fs = firstPos t
           chs = children t
           ls = lastPos t
 	   (x,y) = getMinMax t
           nfa1 = foldS (updateNode (addIntervalStart name)) nfa fs
           nfa2 = foldr (addIntContinue True) nfa1 (followsWithoutInt ls t)
           nfa3 = foldS (updateNode (addNGIntEnd name x y fs nexts)) nfa2 ls
        in 
                  foldS (updateNode (addReset name)) nfa3 
                               (minusOL (followNodes ls nfa) chs)


addIntervalStart :: Int		 -- the label for the Interval
                    -> NFANode a -- The node (of the nfa) to be updated
		    -> NFANode a -- The updated node

addIntervalStart name node
         = changeInts (IntStart name:getInts node) node
  
 

addMinEnd ::    Int	-- the label for the interval
             -> Int	-- the minimum number of times we have to match
             -> OrdList Node -- all the first nodes of the interval
             -> OrdList Node -- all the follow nodes, excluding those that
                             -- are formed from inside interval
             -> NFANode a -- The node (of the nfa) to be updated
	     -> NFANode a -- The updated node

addMinEnd name min fs nexts node =
  changeInts ((getInts node)++[MinEnd name min fs nexts]) node
      

addNGMinEnd :: Int	  -- the label for the interval
               -> Int	  -- the minimum number of times we have to match
               -> OrdList Node -- all the first nodes of the interval
               -> OrdList Node -- all the follow nodes, excluding those that
                               -- are formed from inside interval
               -> NFANode a -- The node (of the nfa) to be updated
	       -> NFANode a -- The updated node

addNGMinEnd name min fs nexts  node =
  changeInts ((getInts node)++[NGMinEnd name min fs nexts]) node

addReset ::    Int	-- the label for the interval 
            -> NFANode a -- The node (of the nfa) to be updated
	    -> NFANode a -- The updated node
             

addReset name node
     = changeInts (Reset name:getInts node) node
                 -- always want to Reset. as will never reset on
                 -- node that started min interval, can't do harm to
		 -- put it first


addIntEnd :: Int	 -- the label for the interval
             -> Int	 -- the min number of times we can loop	
             -> Int	 -- the max number of times we can loop
             -> OrdList Node -- all the first nodes of the interval
             -> OrdList Node -- all the follow nodes, excluding those that
                             -- are formed from inside interval
             -> NFANode a -- The node (of the nfa) to be updated
	     -> NFANode a -- The updated node
             

addIntEnd name min max fs nexts  node
     = changeInts ((getInts node)++[IntEnd name min max fs nexts]) node


addNGIntEnd :: Int	 -- the label for the interval
             -> Int	 -- the min number of times we can loop	
             -> Int	 -- the max number of times we can loop
             -> OrdList Node -- all the first nodes of the interval
             -> OrdList Node -- all the follow nodes, excluding those that
                             -- are formed from inside interval
             -> NFANode a -- The node (of the nfa) to be updated
	     -> NFANode a -- The updated node

addNGIntEnd name min max fs nexts  node
     = changeInts ((getInts node)++[NGIntEnd name min max fs nexts]) node



addIntContinue :: Bool		-- whether it's non-greedy
               -> (Node,[Node]) -- the node to modify & all follow nodes of it
               -> NFA a		-- the nfa to update
               -> NFA a		-- the updated nfa

addIntContinue non_greedy (nodeLbl,nexts) nfa =
  updateNode (\node -> (changeInts ((getInts node) ++[newint]) node))
             nodeLbl nfa
 where
    newint = if non_greedy then
               NGIntContinue nexts
             else
               IntContinue nexts
   
