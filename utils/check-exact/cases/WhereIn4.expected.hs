module WhereIn4 where

--A definition can be demoted to the local 'where' binding of a friend declaration,
--if it is only used by this friend declaration.

--Demoting a definition narrows down the scope of the definition.
--In this example, demote the top level 'sq' to 'sumSquares'
--In this case (there is single matches), if possible,
--the parameters will be folded after demoting and type sigature will be removed.

sumSquares x y = sq p x + sq p y
         where p_2=2  {-There is a comment-}

sq::Int->Int->Int
sq pow z = z^pow  --there is a comment

anotherFun 0 y = sq y
     where  sq x = x^2

