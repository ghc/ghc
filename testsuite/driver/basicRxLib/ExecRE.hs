module ExecRE 
      (execRegexp,
       ReturnREM)
where

import OrdList
import FiniteMap
import Array
import List
import NFADefs

{- A REM contains matchinfo used when running the nfa, nfaRun returns
a list of them. If there is no altered interval or subexp info, &
every match is of the same length then said list will be of length 1,
otherwise we can take the first of them, which reaches a totally final
state. This makes the match in a totally perlesque manner.
-}
 
type REM a = (OrdList Node,	-- current nodes 
              ([a],	-- what matched
               [a],	-- what's left to match	
               Altered (Intervals,   -- state of all interval matches
                        Subexps a))) -- state of all subexp matches

     
mkInitrem :: [REM a]	-- takes an initial
          -> [a]	-- and rest of input
          -> [REM a]	--  adds "rest of input" info
mkInitrem [(ns,(ms,[],other))] xs = [(ns,(ms,xs,other))]  


type ReturnREM a = Maybe (FiniteMap String [a], -- subexp matchinfo
                          [a],			-- what's before the match
                          [a],			-- what matched
                          [a])			-- what's after the match

type Intervals = Array Int Interval  -- state of all interval matches
data Interval = NILL	-- not entered interval yet
              | IN Int	-- IN n: still inside interval, 
			--	 and have matched n times,
              | OUT	-- have matched the minimum number of times
			-- required for a min interval
              deriving Eq

type Subexps a = FiniteMap String (SE a) -- state of all subexp matches

data SE a = OPEN [a]	-- OPEN xs: entered subexp, and matched xs so far
          | CLOSED [a]	-- CLOSED xs: finished subexp, and matched xs

data Altered a = OLD a	-- subexp or interval info is unchanged
               | NEW a	-- subexp or interval info has changed

extractcore :: Altered a -> a
extractcore (OLD a) = a
extractcore (NEW a) = a

execRegexp :: (Show a,Read a,Eq a) =>  
                   (NFA a,      -- the nfa
                    Node,	-- the final node of the nfa
                    [Node],	-- the initial nodes of the nfa
                    Int,	-- number of intervals in nfa
		    Bool)	-- whether we're just matching a prefix

		-> [a]		-- input list
		-> ReturnREM a	-- either "Nothing", or "Just" the 
				-- match results

execRegexp (nfa,final,inits,numintervals,pre) inp =
    let
	initSE = emptyFM 
	initIntervals = listArray (0,numintervals) (repeat NILL)	
	initREM = [(inits,([],[],OLD (initIntervals,initSE)))]
        resultses ses = (mapFM change.snd.extractcore) ses

    in 
       case (runNFA nfa final initREM (mkInitrem initREM inp) [] inp pre) of
         ([],[]) -> Nothing
         (((_,(whole,after,ses)):xs),before) ->
              Just (resultses ses,reverse before,
                    reverse whole,after)
      

change :: String -> SE a -> [a]
change _ (OPEN xs) = xs
change _ (CLOSED xs) = xs
 
runNFA :: (Eq a) =>  
                   NFA a        -- the nfa
		-> Node		-- the final node of the nfa
		-> [REM a]	-- the initial REM info, for
				-- restarting search
		-> [REM a]	-- the current REM info
		-> [a]		-- everything before the start of the
				-- current match
		-> [a]		-- everything after the start of the
				-- current match
		-> Bool		-- whether we're just matching a prefix

		-> ([REM a],-- the new REM info, after this step
                    [a])	-- what's in the input list before the
				-- current match
{- runNFA -
     we step through the nfa. A REM stores match info, and current nodes. 
     We are therefore currently in all the states stored in the REM's.
     All the nodes in one REM state have matched exactly the same
     thing, and have the same interval information. 
     We keep matching until we reach a purely final state, or until
     a match fails. If we were to stop when one of the initial states
     was a final one, then we'd stop after a repeatable thing had
     matched only once. Eg a* will initially be in a final state, as it can
     match 0 times. 

     We don't stop when there is no input left. Instead we try
     matching again! Why? Assertions are full nodes. They may have to
     be applied at the end of a match, when there is no input left.
     eg a$ against "a". after matching the "a" node there's no input
     left. But we wish to try firing the "$" (suffix) node.
     Assertions are usually the only nodes that can fire against empty
     input. A user defined matcher function could match empty input,  
     this case is ignored so runNFA could loop infinitely. Eg a<<1>>*
     where function 1 matches the empty list would loop, as we keep
     trying to match <<1>> as it's repeatable, and it never fails.
     But then this would happen whatever we do. 

     Order matters. We're only interested in when the head of the REM
     list reaches a final state. If we put left's of "|" before right,
     and move to beginning of repeatable expression before moving out
     of it then we have fulfilled the matching semantics. Non-greedy
     nodes simply put results of moving out of interval before those of
     moving to start of interval.
-}

runNFA nfa final initrem rems@((ns,(_,as,_)):_) before rest prefix 
        | ns ==[final] = (rems,before) 
        -- in only final state so definitely finished
	-- if we wanted to do full longest matching then we should not stop
	-- until all states are final

runNFA nfa final initrem rems@((ns,(_,as,_)):_) before rest prefix
        = case (foldr (stepSingleREM nfa final before) [] rems) of
           [] -> case (filter ((final `elem`). fst) rems) of
                   [] -> if prefix then
                          ([],[])
                         else 
                          case rest of
                           [] -> ([],[]) -- no more to match against 
                           (r:rs) ->     -- there is input left so try
                             runNFA nfa final initrem (mkInitrem initrem rs) (r:before) rs prefix
                                                     -- try matching from next
						     -- position
                   (rem:_) -> ([rem],before)
           newrems@(y:ys) ->
              runNFA nfa final initrem newrems before rest prefix
            
           




stepSingleREM :: (Eq a) => 
                    NFA a	-- the nfa
                 -> Node	-- the final node of the nfa
                 -> [a]		-- everything before the start of
				-- current match
                 -> REM a	-- the current REM we're stepping on
                 -> [REM a]	-- the results of REM steps so far
                 -> [REM a]	-- the new results of REM steps

stepSingleREM nfa final before (cns,(ms,as,extrabit)) sofar
     = (foldS (combine (nfa,(ms,as,extractcore extrabit)) before) [] cns)
       ++ sofar
       -- step each node, from the current list & combine results

   where
     combine (nfa,stuff@(_,_,oldextra)) before  n newrems
         = case (nfaStep nfa stuff before n) of

             [] -> newrems -- that node did not match

             (rem@(ns,(ms,as,OLD _)):[]) -> -- node matched & subexp
                                            -- and interval info unchanged 
                                            -- by stepping it through
                 case newrems of
                     [] -> [rem]  -- tis the first REM so just 
                     all@((lastns,(lastms,_,OLD _)):xs) -> 
                        (unionOL ns lastns,(ms,as,OLD oldextra)):xs
                             -- this and last REM matched the same
                             -- thing so combine them
                     all@(x:xs) -> rem:all
                             -- last REM did something fancy so just
                             -- add new one to front
             rems -> 
                 rems ++ newrems
                             -- this REM did something fancy so add it to front

nfaStep :: Eq a => 
              NFA a		-- the nfa
           -> ([a],		-- what matched so far
               [a],		-- what's left to match
               (Intervals,	-- interval info
                Subexps a))	-- subexp info
	   -> [a]		-- what's before the match
           -> Node		-- the current node we're going to step
           -> [REM a]		-- the results (possibly several for several)

nfaStep nfa (whole,after,extra@(intervals,subexps)) before x
     = case nfa!x of			-- get the node
        (Node n next ints ses) ->	
         let bef = whole ++ before 
         in
          case (fire n bef after subexps) of 
                                        -- fire it on current input
            Nothing -> []		-- it failed to match anything
            (Just (ms,as)) ->			-- it did match 
             let newms = (reverse ms) ++ whole	-- we're returning
		 				-- match back to front

		 -- handle changes to subexps and intervals
		 -- no subexp or interval changes here
                 dealfancy [] [] =
	            -- if length ms >1 then we've got something else
		    -- fancy so just tell em we've got NEW stuff
                    if length ms == 1 then
                      [(next,(newms,as,OLD extra))]
                    else 
                      [(next,(newms,as,NEW extra))]

		 -- interval changes only
                 dealfancy (x:xs) [] 
                       = case (dealInts ints (intervals,next)) of
                           s -> 
                             map (dointerval newms as subexps) (nub s)

		 -- subexp changes only                       
                 dealfancy [] (y:ys) 
                       = case (dealSubexps1 ses subexps ms next) of
                           newsubs ->
                             map (dosubexp newms as intervals) newsubs


		 -- interval and subexp changes
                 dealfancy (x:xs) (y:ys)
                       = case (dealInts ints (intervals,next)) of
                          
                          s ->
			    concat 
                             (map (\(ints,ns)->
                                  map (dosubexp newms as ints) 
                                      (dealSubexps1 ses subexps ms ns)) 
                                  (nub s))
                                 

		 dosubexp newms as ints (subs,nexts) = 
                                   (nexts,(newms,as,NEW (ints,subs)))
                 dointerval newms as subs (ints,nexts) =
                                   (nexts,(newms,as,NEW (ints,subs)))

             in
                dealfancy ints ses

dealInts ::  
               [IntervalInfo]	-- interval changes fired by node 
            -> (Intervals,[Node])
            -> [(Intervals,	-- new intervals
                [Node])]	-- new possibly changed next nodes
			
dealInts [] (intervals,ns) = [(intervals,ns)]
dealInts (i:intRules) (intervals,ns) =
            case (handle  i ns intervals) of

              {- got an IntContinue from handle, we therefore store
                 the current interval state, with the next nodes.
                 when we come back round we'll try finishing
                 the interval in them.
                 If greedy we store state first so that we match longest. 
                 (ie end interval in last place possible)
                 otherwise (non-greedy) store state second so that try
                 with this iteration of interval ending here first
              -}
              (_,Nothing,non_greedy,Just ns1,_) ->
                 if non_greedy then
                   (dealInts intRules (intervals,ns))++[(intervals,ns1)]
                 else
                   (intervals,ns1):(dealInts intRules  (intervals,ns))

              -- done some form of interval start or end 
              (x,Just el,non_greedy,nodes1,nodes2) -> 
                 let newints = intervals // [(x,el)]
                 in
                  case (nodes1,nodes2) of

                    -- done an interval start, so store nothing but move on
                    (Nothing,Nothing) -> 
                        dealInts intRules  (newints,ns)

                    -- not looped min times yet, store this state, but
                    -- don't move on 
                    (Just ns1,Nothing) -> 
                        [(newints,ns1)]

                    -- looped max times, so don't store state yet. instead
                    -- try passing on to higher intervals
                    (Nothing,Just ns2) -> 
                        dealInts intRules (newints,ns2)

                    {- between min & max, or got min only interval
                       either way store the state (going back to start
                       of interval) and move on to higher intervals
                       non-greedy move on to higher ones first, 
                       greedy move to higher ones second, so we try
                       continuing this interval as many times as possible first
                    -}
                    (Just ns1,Just ns2) -> 
                        if non_greedy then
                         (dealInts intRules  (newints,ns2))++[(newints,ns1)]
                        else 
                         (newints,ns1):(dealInts intRules (newints,ns2))

    where    

             
             handle ::  
                          IntervalInfo -- interval change we're trying
                       -> [Node]    -- what the next nodes are supposed to be
                       -> Intervals -- current intervals
                       -> (Int,	      -- interval to update
                           Maybe Interval, -- what to update it with
                           Bool,        -- non-greedy
                           Maybe [Node],-- nodes to record
	                   Maybe [Node])-- nodes if leaving an interval 
			       	        -- or mid


             -- just reset interval x, to NILL
             handle  (Reset x) ns intervals =
                                    (x,Just NILL,False,Nothing,Nothing)

             -- start up interval x, if the lookbehind assertion holds
             -- if it's already started then do nothing
             handle (IntStart x) ns intervals = 
                          case (intervals!x) of
                            NILL -> (x,Just (IN 1),False,Nothing,Nothing)
                                   
                            (IN y) -> (x,Just (IN y),False,Nothing,Nothing)
                            OUT ->   (x,Just OUT,False,Nothing,Nothing)


             -- continue a greedy interval, by sending back the follow nodes
             -- of this one, then we store current interval state
             handle (IntContinue ns1) ns 
                    intervals =
                     (error "nothing",Nothing,False,Just ns1,Nothing)

             -- continue a greedy interval, by sending back the follow nodes
             -- of this one, then we store the current interval state
             handle (NGIntContinue ns1) ns 
                    intervals =
                      (error "nothing",Nothing,True,Just ns1,Nothing)

             -- end a minmax interval, if we've not looped enough then just
	     -- just try moving to ns1 (start), if we're between min & max 
             -- try moving to ns1 (start) & ns2 (outside), ie send both back
             -- if we've matched max times send back ns2 (outside) 
             handle (IntEnd x min max ns1 ns2)  ns 
                    intervals =
                      case (intervals!x) of
                        NILL -> error "NILL in IntEnd"
                        (IN y) ->
                                if y < min then
                                   (x,Just (IN (y+1)),False,Just ns1,Nothing)
                                else if (y>=min && y<max) then
                                   (x,Just (IN (y+1)),False,Just ns1,Just ns2)
                                else -- y > max
                                  (x,Just NILL,False,Nothing,Just ns2)
                               
                        OUT -> error "out in IntEnd"

             -- end a min interval, if we've not looped enough then just
	     -- just try moving to ns1 (start), (send ns1 back)
             -- if we've matched min times try moving to start & to outside
	     -- send back ns1 (start) & ns2 (outside) 
             handle (MinEnd x min ns1 ns2) ns  
                    intervals =
                     
                          case (intervals!x) of 
                              NILL -> error "NILL in MinEnd"
                              (IN y) -> if y < min then
                                         (x,Just (IN (y+1)),False,Just ns1,Nothing)
                                        else
                                          (x,Just OUT,False,Just ns1,Just ns2)
                              OUT -> (x,Just OUT,False,Just ns1,Just ns2)
                                   

                {- 
                   ns2 is nodes after end of interval, ns1 start of interval
                   so want to jump to ns2 in preference to ns1 
                -}

             -- end a min interval, if we've not looped enough then just
	     -- just try moving to ns1 (start), (send ns1 back)
             -- if we've matched min times try moving to start & to outside
	     -- send back  ns2 (outside) & ns1 (start) 
             handle (NGMinEnd x min ns1 ns2) ns
                    intervals =

                       case (intervals!x) of 
                         NILL -> error "NILL in MinEnd"
                         (IN y) -> if y < min then
                                    (x,Just (IN (y+1)),True,Just ns1,Nothing)
                                   else 
	   			  
                                     (x,Just OUT,True, Just ns1, Just ns2)
		              	 
                         OUT -> (x,Just OUT,True, Just ns1, Just ns2)
                              
             -- end a minmax NGinterval, if we've not looped enough then just
	     -- just try moving to ns1 (start), if we're between min & max 
             -- try moving to ns2 (outside) & ns1 (start), ie send both back
             -- if we've matched max times send back ns2 (outside) 
             handle (NGIntEnd x min max ns1 ns2) ns 
                     intervals =
                          case (intervals!x) of
                               NILL -> error "NILL in IntEnd"
                               (IN y) -> 
                                     if y < min then
                                      (x,Just (IN (y+1)),True,Just ns1,Nothing)
                                     else if (y>=min && y<max) then
                                      (x,Just (IN (y+1)),True,Just ns1,Just ns2)
                                     else -- y >= max
                                       (x,Just NILL,True,Nothing,Just ns2)
                                  
                               OUT -> error "out in IntEnd"



fire :: Eq a => NFAElem a   -- the matching function wrapped in an NFA node
            -> [a]	    -- everything before start of this match
	    -> [a]	    -- list to match against
            -> Subexps a    -- the subexp info (incase of backreferencing)
            -> 
                Maybe ([a], -- what matched  
                       [a])-- what's left over

fire (NFABack x f) before inps subexps  = 
     case getsubexp (lookupFM subexps x) of
      Nothing -> Nothing
      (Just ms) -> f ms before inps
     

fire (NFAEl f) before inps  _ = f before inps

fire NFAFinal before inps  _ = Just ([],inps)


getsubexp:: Maybe (SE a)-- a subexpression
            -> Maybe [a]-- what it's match contains, or nothing if its
			-- not been initialised
-- both errors should have been caught at initial parser level
getsubexp Nothing    = Nothing	-- can happen, eg ((a)|(b))\\3 against "aa"
				-- b don't match anything, so is still NOUGHT
getsubexp (Just (OPEN as)) = Just as-- can happen eg ((a){2})\\1 as
				-- closed at 2nd (\\1) node, and subexp
				-- stuff done AFTER matching 
getsubexp (Just (CLOSED as)) = Just as
{-
dealsubexps :: [SubexpInfo] -- subexp alterations from this node
               -> Subexps a -- the current subexps status
               -> [a]	    -- what to match against
               -> Subexps a -- the new subexps status

dealsubexps [] subexps ms = subexps

dealsubexps (s:ses) subexps ms  
   =   let 
         updateSubStart el1 (OPEN ms) = 
               case el1 of                    -- subexp's current status is
                    OPEN xs -> OPEN (xs ++ ms)-- currently open so add more
                    CLOSED xs -> OPEN ms      -- closed so restart it
         updateSubMid el1 (OPEN ms) =
               case el1 of	-- subexp's current status is
                 OPEN xs -> OPEN (xs ++ ms)   -- currently open so add more
                 CLOSED xs -> OPEN (xs ++ ms) -- closed too early, add more


         newsubexps =
             case s of
              (NFASubStart x) -> -- start a subexpression
                  addToFM_C updateSubStart subexps x (OPEN ms)
                      -- default is to just newly open it
              (NFASubMid x) -> -- in middle of a subexpression
                  addToFM_C updateSubMid subexps x (OPEN ms)
                      -- default should never be needed.

              (NFASubEnd x) -> -- close a subexpression
                case (lookupFM subexps x) of -- subexp's current status is
                 Nothing  -> subexps     -- unstarted, leave it 
                                         -- (eg match (a*)b against b,
					 -- a never fires, and so will
					 -- get Nothing in b
                 (Just (OPEN xs)) -> addToFM subexps x (CLOSED xs) 
                                         -- open so close it
                 (Just (CLOSED xs)) -> addToFM subexps x (CLOSED xs) 
                                         -- its closed so keep it closed

       in
                newsubexps --dealsubexps ses newsubexps ms
-}


dealSubexps1 :: [SubexpInfo]  -- subexp alterations from this node
               -> Subexps a   -- the current subexps status
               -> [a]	      -- what to match against
               -> [Node]      -- the next nodes, need to divide up to
			      -- prevent premature closure
               -> [(Subexps a,-- the new subexps status
		   [Node])]

dealSubexps1 [] subexps ms ns = [(subexps,ns)]

dealSubexps1 (s:ses) subexps ms ns 
   =   let 
         updateSubAdd el1 (OPEN ms) = 
               case el1 of                    -- subexp's current status is
                    OPEN xs -> OPEN (xs ++ ms)-- currently open so add more
                    CLOSED xs -> OPEN ms      -- closed so restart it

         (newsubexps, newnodes) =
             case s of
              (NFASubAdd x) -> -- add match info to a subexpression
                  (addToFM_C updateSubAdd subexps x (OPEN ms),Nothing)
                      -- default is to just newly open it

	      (NFASubContinue nexts) -> -- this node may close a subexp
		-- but there are others (nexts) after it, that could also 
		-- do so. Make sure we don't close it prematurely.
		-- Need this complexity to allow things like (a*b*)* to work.
                   (subexps,Just (nexts `intersectOL` ns,ns `minusOL` nexts))    

              (NFASubEnd x) -> -- close a subexpression
                case (lookupFM subexps x) of -- subexp's current status is
                 Nothing  -> (subexps,Nothing)
					 -- unstarted, leave it 
                                         -- (eg match (a*)b against b,
					 -- a never fires, and so will
					 -- get Nothing in b
                 (Just (OPEN xs)) -> (addToFM subexps x (CLOSED xs),Nothing) 
                                         -- open so close it
                 (Just (CLOSED xs)) -> (addToFM subexps x (CLOSED xs),Nothing) 
                                         -- its closed so keep it closed

       in
            case newnodes of
              Nothing -> dealSubexps1 ses newsubexps ms ns
	      (Just (ns1,ns2)) -> 
                    (subexps,ns1):dealSubexps1 ses subexps ms ns2 







