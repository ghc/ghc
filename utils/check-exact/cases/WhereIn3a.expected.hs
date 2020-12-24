module WhereIn3a where

-- A leading comment
anotherFun 0 y = sq y
     where  sq x = x^2

sq pow 0 = 0      -- prior comment
sq pow z = z^pow  --there is a comment

--A definition can be demoted to the local 'where' binding of a friend declaration,
--if it is only used by this friend declaration.

--Demoting a definition narrows down the scope of the definition.
--In this example, demote the top level 'sq' to 'sumSquares'
--In this case (there are multi matches), the parameters are not folded after demoting.

sumSquares x y = sq p x + sq p y
         where p=2  {-There is a comment-}

sq :: Int -> Int -> Int
sq pow 0 = 0      -- prior comment
sq pow z = z^pow  --there is a comment

-- A leading comment
anotherFun 0 y = sq y
     where  sq x = x^2

