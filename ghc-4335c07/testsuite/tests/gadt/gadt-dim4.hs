{-# LANGUAGE GADTs #-}

module ShouldSucceed1 where 

data T1 a where 
  C :: (T2 b) -> b -> T1 Int 
  D :: Bool -> T1 Bool 

 -- should this work? 
data T2 a where 
   F :: Int -> T2 Int


-- Should work, even though we start as wobbly 
-- the existential makes us rigid again 
foo x = case x of 
          C (F _) z -> (z + 1) 




