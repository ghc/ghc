{-# LANGUAGE GADTs #-}

module TrimCpr where

expensive :: Int -> Bool
expensive = even
{-# NOINLINE expensive #-}

-- | Should *not* have the CPR property, because it has arity 1.
trimByArity :: Int -> Int -> Maybe Int
trimByArity x
  | expensive x = \y -> Just (x+y)
  | otherwise   = \y -> Just y
{-# NOINLINE trimByArity #-}

data T a where
  A :: T Int
  B :: T (Int -> Int)

-- | If we say `LLb` for 'g', this will get the CPR property, which is wrong.
trimByArity2 :: Int -> Int -> Int
trimByArity2 0 y = y + 1
trimByArity2 x y = g B (x+y)
  where
    g :: T a -> a
    g A = error "A"
    g B = \_ -> error "B"
    {-# NOINLINE g #-}

-- | Should *not* have the CPR property, because evaluation of `blah` won't
-- terminate and `f` won't have the nested CPR property.
trimByTerm :: Int -> Int
trimByTerm n
  | n < 0     = n
  | otherwise = case f n of Just blah -> blah
  where
    f :: Int -> Maybe Int
    f n = Just (sum [0..n])
    {-# NOINLINE f #-}
{-# NOINLINE trimByTerm #-}
