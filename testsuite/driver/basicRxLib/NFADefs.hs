module NFADefs where


import OrdList
import BagRE
import Matchers
import FiniteMap
import Array

type Node = Int	-- an NFA node is referenced by an integer

type NFA a = Array Int (NFANode a) -- The NFA itself

data NFAElem a = 
                 NFAEl (MatcherImpl a) -- matcher function 

               | NFABack String	   -- matches same as the nth subexpression
                         ([a] -> MatcherImpl a)-- the function to use to match
                                
               | NFAFinal	   -- Final state of NFA

               
{- NFASubAdd n   -> we are inside of subexp n,  so add match info to it
   NFASubContinue ns -> we're at a possible end of the subexp, but
                        don't close it prematurely, try closing it in
                        the other nodes as well.
   NFASubEnd n	 -> we have now left subexp n, so close it down
-}

data SubexpInfo = NFASubAdd String
                | NFASubContinue (OrdList Node)
                | NFASubEnd String


{- IntStart n			-> we have now entered the nth Interval 

   IntEnd n min max ns1 ns2	-> we have now reached the end of an
				   iteration of the nth Interval,
                                   it's a MinMax interval,
                                   min the min number of times we can loop,
                                   max is the max number of times we can loop
                                   ns1, nodes go to before reaching min
                                        firspos nodes of interval
                                   ns2, nodes go to after reaching max
                                        lastpos nodes of interval

   MinEnd n min ns1 ns2	-> we have now reached the end of an iteration
			   of the nth interval,
                           it's a Min interval,
			   min is the min number of times we can loop,
			   ns1, nodes we can go to before reaching min
                                firspos nodes of interval
                           ns2, last nodes of the interval
                                lastpos nodes of interval


   Reset n		-> we have now finished with the nth interval,
			   by moving on to the following node. We
			   want to reset it, in case we
			   come back to the start of it later. 
                           (Important if we've got nested intervals)


   NGIntEnd n min max ns1 ns2	-> As for IntEnd above, except we're
				   doing non-greedy matching. When
				   between min and max matches, we
				   therefore want to try jumping out
				   of the interval before we try
				   continuing it. More on this in
				   ExecRE module.

   NGMinEnd n min max ns1 ns2 -> Non-Greedy MinEnd equiv. 
                                 When we've matched min times,
    				 therefore want to try jumping out
				 of the interval before we try
				 continuing it. More on this in
				 ExecRE module.  
-}

data IntervalInfo = IntStart Int 
                  | IntContinue (OrdList Node)
                  | NGIntContinue (OrdList Node)         
                  | IntEnd Int Int Int (OrdList Node) (OrdList Node)
                  | MinEnd Int Int (OrdList Node)  (OrdList Node)
                  | Reset Int
                  | NGIntEnd Int Int Int (OrdList Node) (OrdList Node)
                  | NGMinEnd Int Int (OrdList Node) (OrdList Node)

{- Node n ns is ses ->	NFA state that matches something using "n".
			It will then jump to states "ns".
			Unless node is part of an interval, then
			nextnodes is calculated with "is".
			Node may be part of subexpression, dealt with
			using "ses".
-}

data NFANode a = Node (NFAElem a) (OrdList Node) [IntervalInfo] [SubexpInfo] 



-- building nfa

initNFA :: Int	 -- number of nodes in NFA 
        -> NFA a -- make an initial nfa with the correct number of nodes
initNFA x 
    = listArray (1,x) (repeat (Node NFAFinal emptyOL [] []))

mkNfaBack :: String	        -- which subexp to refer to  
          -> ([a] -> MatcherImpl a) -- the matcher function to use
          -> NFANode a		-- current node
          -> NFANode a          -- an NFA node that does this
mkNfaBack x f (Node _ nexts ses ints)
    = Node (NFABack x f) nexts ses ints

mkNfaEl :: MatcherImpl a	-- a matcher function for simple element
        -> NFANode a		-- current node
        -> NFANode a	-- a simple NFA element 
mkNfaEl f (Node _ nexts ses ints)
    = Node (NFAEl f) nexts ses ints

mkNfaFinal :: Int	-- the label of the final node
           -> NFANode a		-- current node
           -> NFANode a -- a final node, that jumps back to itself if fired
mkNfaFinal n (Node _ _ ses ints) = Node NFAFinal (singletonOL n) ses ints 


-- updating nfa

updateNode :: (NFANode a -> NFANode a) --updating function
           -> Node	-- label of node to update
           -> NFA a	-- nfa to update
           -> NFA a	-- updated nfa
updateNode f el nfa =  nfa // [(el, f (nfa!el))]

alterNode  :: NFANode a -- new nfa node
           -> Node	-- label of node to update
           -> NFA a	-- old nfa
           -> NFA a	-- updated nfa
alterNode newnode el nfa =  nfa // [(el, newnode)]

changeNexts :: [Node]	 -- the new next nodes 
            -> NFANode a -- the actual nfa node to update
            -> NFANode a -- that updated node
changeNexts newnexts (Node n _ int se) = Node n newnexts int se

changeInts :: [IntervalInfo] -- the new interval info 
           -> NFANode a	     -- the actual nfa node to update
           -> NFANode a      -- that updated node
changeInts newint (Node n nexts _ se) = Node n nexts newint se

changeSES :: [SubexpInfo] -- the new subexp info
          -> NFANode a	  -- the actual nfa node to update
          -> NFANode a	  -- that updated node
changeSES newses (Node n nexts int _) = Node n nexts int newses

 
-- interrogating the nfa

getNfaElem :: NFANode a -> NFAElem a
getNfaElem (Node n _ _ _) = n

getNexts :: NFANode a -> [Node]
getNexts (Node _ nexts _ _) = nexts

getInts :: NFANode a -> [IntervalInfo]
getInts (Node _ _ ints _) = ints

getSES :: NFANode a -> [SubexpInfo] 
getSES (Node _ _ _ ses) = ses


followNodes :: OrdList Int        -- some nodes of an nfa
             -> NFA a   -- the nfa
             -> OrdList Int -- all the nodes that follow after those nodes

followNodes s nfa = foldS (combine nfa) emptyOL s
                 where
                     combine nfa e s1 = unionOL (after (nfa!e)) s1
                     after (Node _ xs _ _)  = xs

