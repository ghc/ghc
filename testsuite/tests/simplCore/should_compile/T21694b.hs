module T21694 where

-- f should get arity 4
f x = let j 0 = \ a b c -> (a,x,b)
          j n = j (n-1 :: Int)
      in j 20
