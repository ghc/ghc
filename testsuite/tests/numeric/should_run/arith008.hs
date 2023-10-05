-- !!! a random test from Nick North
-- (got this in mid-1993; don't remember why.  WDP 95/02)
--

random_numbers :: (Int, Int, Int) -> [Float]
random_numbers (s1,s2,s3)
    =  map (snd . properFraction . combine) (iterate f (s1,s2,s3))
       where
       combine :: (Int,Int,Int) -> Float
       combine (a,b,c)  =
            fromIntegral(a)/30269 + fromIntegral(b)/30307
            + fromIntegral(c)/30323
       f (a,b,c)  =
           ((171*a) `mod` 30269, (172*b) `mod` 30307, (170*c) `mod` 30323)

-- partain: changed to cvt spaces into newlines (easier to see bugs)
-- sof: define approp. version of showList to do this.
main = putStr (showL (showsPrec 0) (take 1000 (random_numbers (9807, 65, 32975))) "\n")

showL showx []     = showString "[]"
showL showx (x:xs) = showChar '[' . showx x . showl xs
  where
    showl []     = showChar ']'
    showl (x:xs) = showString ",\n" . showx x . showl xs
