{-# LANGUAGE LinearTypes #-}

-- This is the original reproducer for #24725. If this test fails and the
-- shorter T24725 doesn't, then we've found a new way of failing this test.

module ConcatMap where

data Stream a = forall s. Stream (s -> Step s a) !s
data Step s a = Yield a !s | Skip !s | Done

data Tuple a b = a :!: b
data Option a = None | Some !a

concatMapS :: (a -> Stream b) -> Stream a -> Stream b
concatMapS f (Stream next0 s0) = Stream next (s0 :!: None)
  where
    {-# INLINE next #-}
    next (s :!: None) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip (s' :!: None)
      Yield x s' -> Skip (s' :!: Some (f x))

    next (s :!: Some (Stream g t)) = case g t of
      Done       -> Skip    (s :!: None)
      Skip    t' -> Skip    (s :!: Some (Stream g t'))
      Yield x t' -> Yield x (s :!: Some (Stream g t'))
{-# INLINE [1] concatMapS #-}

concatMapS' :: (s -> Step s b) -> (a -> s) -> Stream a -> Stream b
concatMapS' next2 f (Stream next1 s0) = Stream next (s0 :!: None)
  where
    {-# INLINE next #-}
    next (s :!: None) = case next1 s of
      Done       -> Done
      Skip    s' -> Skip (s' :!: None)
      Yield x s' -> Skip (s' :!: Some (f x))

    next (s :!: Some t) = case next2 t of
      Done       -> Skip    (s :!: None)
      Skip    t' -> Skip    (s :!: Some t')
      Yield x t' -> Yield x (s :!: Some t')
{-# INLINE concatMapS' #-}

{-# RULES "testedRule" forall step f. concatMapS (\x -> Stream step (f x)) = concatMapS' step f #-}

replicateStep1 :: Tuple Int a -> Step (Tuple Int a) a
replicateStep1 (0 :!: _) = Done
replicateStep1 (n :!: x) = Yield x ((n - 1) :!: x)

replicateS1 :: Int -> a -> Stream a
replicateS1 n x = Stream replicateStep1 (n :!: x)
{-# INLINE replicateS1 #-}

should_fire :: Stream Int -> Stream Int
should_fire = concatMapS (replicateS1 2)
