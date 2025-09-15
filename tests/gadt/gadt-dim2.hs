{-# LANGUAGE GADTs #-}

module ShouldFail2 where 

data T a where 
  C :: Int -> T Int 
  D :: Bool -> T Bool 

-- should fail because variable is wobbly 
foo (C x) = x 
foo (D b) = b 
