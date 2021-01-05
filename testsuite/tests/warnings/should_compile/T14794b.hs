{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wmissing-signatures -Wmissing-exported-signatures #-}

module T14794b (test1, pattern Test2, test3, pattern Test4) where

-- This should generate warnings with;
-- -Wmissing-signatures -Wmissing-exported-signatures

test3 = True

pattern Test4 <- True

test7 = True

pattern Test8 <- True


-- This should not generate warnings with;
-- -Wmissing-signatures -Wmissing-exported-signatures

test1 :: Bool
test1 = True

pattern Test2 :: Bool
pattern Test2 <- True

test5 :: Bool
test5 = True

pattern Test6 :: Bool
pattern Test6 <- True
