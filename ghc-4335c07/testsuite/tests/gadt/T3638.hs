{-# LANGUAGE GADTs #-}

module T3638 where

data T a where TInt :: T Int

foo :: T Int -> Int
{-# NOINLINE [1] foo #-}
foo TInt = 0

{-# RULES "foo"  forall x. foo x = case x of { TInt -> 0 } #-}
