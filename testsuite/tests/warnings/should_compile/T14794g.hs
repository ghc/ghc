{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wmissing-pattern-synonym-signatures #-}

module T14794g (test1, pattern Test2, test3, pattern Test4) where

-- This should generate warnings with;
-- -Wmissing-pattern-synonym-signatures

pattern Test4 <- True

pattern Test8 <- True


-- This should not generate warnings with;
-- -Wmissing-pattern-synonym-signatures

test1 :: Bool
test1 = True

pattern Test2 :: Bool
pattern Test2 <- True

test3 = True

test5 :: Bool
test5 = True

pattern Test6 :: Bool
pattern Test6 <- True

test7 = True
