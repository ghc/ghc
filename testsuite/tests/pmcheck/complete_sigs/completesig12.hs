{-# LANGUAGE GADTs, PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}
module Completesig11 where
data G a where
  G1' :: G Int
  G2' :: G Bool

pattern G1 :: () => (a ~ Int) => G a
pattern G1 = G1'

pattern G2 :: () => (a ~ Bool) => G a
pattern G2 = G2'

{-# COMPLETE G1, G2 #-}

fa :: G a -> Int   -- exhaustive function
fa G1 = 1
fa G2 = 2

fb :: G Int -> Int -- exhaustive function
fb G1 = 1
-- fb G2 = 2       -- inaccessible clause
