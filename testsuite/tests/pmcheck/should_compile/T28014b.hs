{-# LANGUAGE GADTs, PatternSynonyms, ViewPatterns #-}
module T28014b where

pattern F :: a
pattern F <- (const False -> True)
{-# COMPLETE F #-}

data G a where
  G1' :: G Int
  G2' :: G Bool

data E a where
  E1 :: E Bool

-- | By the COMPLETE pragma, the first clause covers everything.
w :: E a -> G a -> Int
w F _   = 1
w _ G1' = 2
w _ G2' = 3
