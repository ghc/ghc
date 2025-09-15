-- !!! Empty where declarations list
module Mod100 where

y = 3

x = v where v = 2 where

{- Example of a nested context not being further
   indented than the enclosing - an error (from the
   Sec B.2 of the H98 report).

f x = let
       h y = let
     p z = z
             in p
      in h

-}

f e = let { x = e; y = x } in y

z = 'a' where 

class A a where

instance A Int where

instance A Char
