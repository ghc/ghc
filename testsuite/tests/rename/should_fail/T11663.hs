module T11663 where

-- All of these should fail as type signatures are not allowed
-- in patterns without -XScopedTypeVariables.
hello0 = \(h :: Int) -> print h
hello1 (h :: Int) = print h
hello2 = case 54 of (x :: Int) -> print x
hello4 = case Just 54 of Just (x :: Int) -> print x
