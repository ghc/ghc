module LetIn1 where

--A definition can be demoted to the local 'where' binding of a friend declaration,
--if it is only used by this friend declaration.

--Demoting a definition narrows down the scope of the definition.
--In this example, demote the local  'pow' to 'sq'
--This example also aims to test the demoting a local declaration in 'let'.

sumSquares x y = let sq 0=0
                     sq z=z^pow
                 in sq x + sq y


anotherFun 0 y = sq y
     where  sq x = x^2


