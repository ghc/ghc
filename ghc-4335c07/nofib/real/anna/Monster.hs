
-- ==========================================================--
-- === Monster -- Enumerates the points in a lattice.     ===--
-- === Not part of the strictness analyser proper.        ===--
-- ===                                         Monster.hs ===--
-- ==========================================================--

module Monster where
import BaseDefs
import MyUtils
import Utils
import AbstractVals2
import SuccsAndPreds2
import AbstractMisc

pretty x
  = f 0 0 x
    where
       f n h [] = "\n\nDone: " ++ show n ++ " total points, " ++ show h
                  ++ " height.\n"
       f n h (x:xs)
          = "\n  " ++ show (h+1) ++ " contains " ++ show x
             ++ f (n+x) (h+1) xs

main
   = putStr banner	>>
     getContents	>>= \ inText ->
     let
         d = first (head (reads inText))
     in
     mySeq d (putStr ("\n\n" ++ pretty (map length (amAllUpSlices d))))
   where 
         banner = concat 
                   [ "\nMonster 0.400r: generates all points in a domain.\n",
                     "Copyright (c) Julian Seward 1992",
                     "\n\nEnter the domain: " ]


-- ==========================================================--
-- === end                                     Monster.hs ===--
-- ==========================================================--
