module Test (R(..)) where  

data R = R { x :: Char, y :: Int, z :: Float }
       | S { x :: Char }
       | T { y :: Int, z:: Float }
       | W

