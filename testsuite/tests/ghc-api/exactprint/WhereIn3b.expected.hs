module WhereIn3b where

-- A comment
import Data.List

--A comment after an import 1
--A comment after an import 2

-- A leading comment
anotherFun 0 y = sq y
     where  sq x = x^2

sq pow 0 = 0      -- prior comment
sq pow z = z^pow  --there is a comment

-- A comment immediately preceding a declaration
sumSquares x y = sq p x + sq p y
         where p=2  {-There is a comment-}

sq :: Int -> Int -> Int
sq pow 0 = 0      -- prior comment
sq pow z = z^pow  --there is a comment

-- A leading comment
anotherFun 0 y = sq y
     where  sq x = x^2

-- A comment at the end of the file
