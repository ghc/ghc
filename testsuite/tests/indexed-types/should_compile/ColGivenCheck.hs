{-# LANGUAGE TypeFamilies #-}

module ColInference  where 

type family Elem c

type instance Elem [e] = e

class Col c where
 isEmpty   :: c -> Bool
 add       :: c -> Elem c -> c
 headTail  :: c -> (Elem c,c)

addAll :: (Col c1, Col c2, Elem c1 ~ Elem c2) => c1 -> c2 -> c2
addAll c1 c2 
	| isEmpty c1
	= c2
	| otherwise
	= let (x,c1') = headTail c1
	  in addAll c1' (add c2 x)
