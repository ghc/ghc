module RmDecl1 where

sumSquares x = x * p
         where p=2  {-There is a comment-}

sq :: Int -> Int -> Int
sq pow 0 = 0
sq pow z = z^pow  --there is a comment

{- foo bar -}
anotherFun 0 y = sq y
     where  sq x = x^2

