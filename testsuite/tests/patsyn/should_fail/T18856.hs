{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module T18856 where

pattern P ::  Int -> Bool -> (Int, Bool, [(Bool,Bool)])
pattern P p q <- (q, p, [(True,False)])

