-- {-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# OPTIONS_GHC -O2 -fforce-recomp #-}
-- {-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE MagicHash, UnboxedTuples #-}
module DmdAnalRulesUnfoldings (unf, rule) where

-- g and y are kept alive only through unf's unfolding
-- Do not give them absent demands!
unf :: Int -> Int
unf x = g (x+x)
{-# INLINE unf #-}

g :: Int -> Int
g x = x+y
{-# INLINE[2] g #-}

y :: Int
y = sum [0..42]
{-# NOINLINE[1] y #-}

---------------------------

-- h and z are kept alive only through rule's unfolding
-- Do not give them absent demands!
rule :: Int -> Int
rule x = x
{-# RULES "rule2h" forall x. rule x = h x #-}

h :: Int -> Int
h x = x+z
{-# INLINE[2] h #-}

z :: Int
z = sum [0..43]
{-# NOINLINE[1] z #-}


