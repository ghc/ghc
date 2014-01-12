{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
module PushedInAsGivens where 


type family F a



bar y = let foo :: (F Int ~ [a]) => a -> Int 
            foo x = length [x,y]  
      in (y,foo y)


-- This example demonstrates why we need to push in 
-- an unsolved wanted as a given and not a given/solved.
-- [Wanted] F Int ~ [beta]
--- forall a. F Int ~ [a] => a ~ beta 
-- We we push in the [Wanted] as given, it will interact and solve the implication
-- constraint, and finally we quantify over F Int ~ [beta]. If we push it in as
-- Given/Solved, it will be discarded when we meet the given (F Int ~ [a]) and 
-- we will not be able to solve the implication constraint.



