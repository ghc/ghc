module Fwif where

import Types
import Primitives
import DbParallel

{-
    The following function, fwifdb, implements the friedman-wise 	
    conditional expression. It takes a predicate p, a then branch x and	
    an else branch y. If the branches are identical (using 'eq') then	
    the condition is reduced to the 'then branch' (e.g.). 		
-}


fwifdb :: Bool -> Tree -> Tree -> Tree

-- fwifdb c t f = if c then t else f

fwifdb p x y =
--    case trace "fwifdb" (boolval p) of
    case (boolval p) of
        TRUE ->    x
        FALSE ->   y
        UNKNOWN -> 
	    if eq x y then x
	    else x `seqt` y `seqt`
		 if eq x y then x 
		 else case x of

		      Node1 l1 k1 r1 ->
		            case y of
			         Node1 l2 k2 r2 ->
				       if eq l1 l2 then r' `par` newnode
				       else l' `par` newnode
				       where
					    l' = fwifdb p l1 l2
					    r' = fwifdb p r1 r2
					    newnode = 
						Node1 l'
					     	 (if k1 == k2 then k1 else
					      	  if p then k1 else k2)
					     	 r'
				 _ -> if p then x else y


		      Node2 l1 k11 m1 k21 r1 ->
			    case y of
				 Node2 l2 k12 m2 k22 r2 ->
				       if eq l1 l2 then
					   if eq m1 m2 then
						r' `par` newnode
					   else
						m' `par` newnode
				       else
					   l' `par` newnode
				       where
					   l' = fwifdb p l1 l2
					   m' = fwifdb p m1 m2
				           r' = fwifdb p r1 r2
				           newnode = 
					        Node2 
						    l'
					           (if k11 == k12 then k11 else
					            if p then k11 else k12)
					            m'
					           (if k21 == k22 then k21 else
					            if p then k21 else k22)
					            r'
				 _ ->  if p then x else y


		      Tip e1 ->
			   case y of
				Tip e2 -> Tip (if p then e1 else e2)
			        _ ->      if p then x else y


		      Tip_Acc e1 a1 ->
			   case y of
			        Tip_Acc e2 a2 -> 
					e1 `seqi` a1 `seqi` e2 `seqi` a2 `seqi`
				           Tip_Acc (if p then e1 else e2) (if p then a1 else a2)
				_ ->  if p then x else y

		      _ -> if p then x else y

	_  -> error "Funny BOOL in fwifdb"
