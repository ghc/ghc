{-# OPTIONS_GHC -drule-check concatMap #-}
  -- This rule-check thing crashed #24726

module T24726 where

data Stream a = forall s. Stream (s -> ()) s

concatMapS :: (a -> Stream b) -> Stream a -> Stream b
concatMapS f (Stream next0 s0) = Stream undefined undefined
{-# INLINE [1] concatMapS #-}

concatMapS' :: (s -> ()) -> (a -> s) -> Stream a -> Stream b
concatMapS' = undefined

{-# RULES "concatMap" forall step f. concatMapS (\x -> Stream step (f x)) = concatMapS' step f #-}

replicateStep :: a -> b
replicateStep _ = undefined
{-# INLINE replicateStep #-}

replicateS :: Int -> a -> Stream a
replicateS n x0 = Stream replicateStep undefined
{-# INLINE replicateS #-}

foo1 :: Stream Int -> Stream Int
foo1 = concatMapS (replicateS 2)
