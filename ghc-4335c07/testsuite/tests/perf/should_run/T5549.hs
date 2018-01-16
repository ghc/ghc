module Main where
-- See Trac #5549
-- The issue here is allocating integer constants inside a loop


lcs3 :: Eq a => [a] -> [a] -> [a]
lcs3 a b = fst $ aux (a, length a) (b, length b)
  where
    aux (_,0) _ = ([],0)
    aux _ (_,0) = ([],0)
    aux (a@(ha:as),la) (b@(hb:bs), lb)
      | ha == hb  = let (s,n) = aux (as,la-1) (bs,lb-1) in (ha : s, n+1)
      | otherwise =
        let (sa,na) = aux (as,la-1) (b,lb)
            (sb,nb) = aux (a,la) (bs,lb-1) in
        if na > nb then (sa,na) else (sb,nb)

f :: Integer -> Integer -> Integer
f acc 0 = acc
f acc n = g (acc + 1) (n-1)

g :: Integer -> Integer -> Integer
g acc 0 = acc
g acc n = f (acc -1) (n-1)

main = do putStrLn . show $ f 0 100000000
          putStrLn . show $ lcs3 [1..20] [10..20]
