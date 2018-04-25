module Vtslib( Option(..) , Sum(..) , forall , exists , assoc ,
		haskey , uncurry , curry , for , map' , module Core_datatype )
	      where
	
import Core_datatype

data Option a = NONE | SOME a

data Sum a b = Inl a | Inr b

{-
    (* Apply the predicate p to all the elements of *)
    (* a  list, and then AND the  results together. *)
    N.B. forall & exists rewritten from ML
-}

forall :: ( a -> Bool ) -> [a] -> Bool

forall p = and . ( map p )





{-
    (* Apply the predicate p to all the elements of *)
    (* a list, and  then OR  the results  together. *)
-}

exists :: ( a -> Bool ) -> [a] -> Bool
    
exists p = or . ( map p )





{-
    (* Return the value stored in a association list *)
    (* store  under  key. An  association  list is a *)
    (* list of pairs (key, value)                    *)
-}

assoc :: ( Eq a ) => a -> [(a,b)] -> Option b

assoc key [] = NONE 

assoc key ((key',entry):l') 
	| key == key' = SOME entry
	| otherwise   = assoc key l'






{-
    (* Return whether an association list has a particular key. *)
-}


haskey key al 
	= case assoc key al of
	      SOME _ -> True
	      NONE   -> False




{-
    val op !! = nth

    fun foldl f =
	    let fun fold e [] = e
		  | fold e (a::l) = fold (f a e) l
	    in fold end
		
    fun foldr f e =
	    let fun fold [] = e
		  | fold (a::l) = f a (fold l)
	    in fold end

    exception Reduce

    fun reducel f [a] = a
      | reducel f (a::b::l) = reducel f (f a b :: l)
      | reducel f [] = raise Reduce

    fun reducer f [a] = a
      | reducer f (a::l) = f a (reducer f l)
      | reducer f [] = raise Reduce
-}



{-
    (* Return an uncurried version of a curried function *)
-}

--in 1.3: uncurry f (a,b) = f a b




{-
    (* Return a curried version of an uncurried function *)
-}

--in 1.3: curry f a b = f (a,b)






for :: Int -> (b -> b) -> b -> b

for 0 f y = y
 
for i f y = for (i-1) f ( f y )


{-
    fun str_to_int s =
	    let val s_len = size s
		val zero_ord = fromEnum "0"
		val nine_ord = fromEnum "9"
		fun is_digit i = i >= zero_ord andalso i <= nine_ord
		fun convert si i = 
			if si >= s_len 
			then i 
			else if is_digit (ordof (s,si))  then
				 convert (si + 1) (i * 10 - zero_ord + ordof (s,si))
			else raise Str_to_int
	    in if s_len > 1 then
		   if ordof (s, 0) = fromEnum "~" then
			~ (convert 1 0)
		   else 
			convert 0 0
	       else if s_len > 0 then
			convert 0 0
	       else
			raise Str_to_int
	    end
-}

map' :: (Int -> b -> c) -> [b] -> [c]

map' f = mapf 0 
	 where
         mapf i [] = []

         mapf i (x:l) = f i x : mapf (i+1) l

