{-# LANGUAGE TypeFamilies #-}

module ColInference  where 

type family Elem c

type instance Elem [e] = e

class Col c where
 isEmpty   :: c -> Bool
 add       :: c -> Elem c -> c
 headTail  :: c -> (Elem c,c)

sawpOne c1 c2 
	= let (x,c1') = headTail c1
              (y,c2') = headTail c2
	  in (add c1' y,add c1' x)
