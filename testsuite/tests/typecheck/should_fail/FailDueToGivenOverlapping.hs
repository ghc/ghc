{-# LANGUAGE FlexibleContexts #-}

module FailDueToGivenOverlapping where

class C a where

class D a where 
 dop :: a -> ()


instance C a => D [a] 

-- should succeed since we can't learn anything more for 'a'
foo :: (C a, D [Int]) => a -> () 
foo x = dop [x] 


class E a where 
 eop :: a -> () 

instance E [a] where 
 eop = undefined

-- should fail since we can never be sure that we learnt 
-- everything about the free unification variable.
bar :: E [Int] => () -> ()
bar _ = eop [undefined]
