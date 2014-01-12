
module DfltProb2 where

{- NB: This program fails in GHC 7.4.1 but should be accepted with our new defaulting plan -} 

f :: Int -> Bool
f x = const True (\y -> let w :: a -> a
                            w a = const a (y+1)
                        in w y)

{- The implication constraint we get here is that (y::beta):
            [untch=beta] forall a. 0 => Num beta
   and we can't really do any defaulting in this scope. 

   Instead with our new defaulting plan we 

     (i) try to approximate the whole constraint
     (ii) /then/ we try to default (and succeed)
     (iii) then try to resolve again
 
   See Note [Top-level Defaulting Plan] in TcSimplify -}


