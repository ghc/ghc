{-# LANGUAGE Rank2Types #-}

module SystemF where 
-- System-F examples 


type Sid = forall a. a -> a 

apply :: forall a b . (a -> b) -> a -> b 
apply f g = f g 

hr :: (forall a. a -> a) -> (Int,Bool)
hr f = (f 3,f True)

test0 = apply hr id   -- requires smart-app-arg 

selfApp :: Sid -> Sid 
selfApp x = (x::(Sid -> Sid)) x 



