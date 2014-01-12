module F9 where

f91 = let f = \n -> if n<=100 then f (f (n+11)) else n-10
      in f 10