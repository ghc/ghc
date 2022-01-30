{-# OPTIONS_GHC -O2 -fforce-recomp #-}

-- | From Note [Boxity analysis] and related Notes
module T19871 where

data Huge
  = Huge
  { f1 :: Bool
  , f2 :: Bool
  , f3 :: Bool
  , f4 :: Bool
  , f5 :: Bool
  , f6 :: Bool
  , f7 :: Bool
  , f8 :: Bool
  , f9 :: Bool
  , f10 :: Bool
  , f11 :: Bool
  , f12 :: Bool }

-- | Should not unbox Huge
ann :: Huge -> (Bool, Huge)
ann h@(Huge{f1=True}) = (False, h)
ann h                 = (True,  h)
{-# NOINLINE ann #-}

-- A few examples demonstrating the lubBoxity = unboxedWins tradeoff

-- | Should unbox 'z'.
-- We won't with `lubBoxity = boxedWins`.
-- We will  with `lubBoxity = unboxedWins`.
sumIO :: Int -> Int -> IO Int
sumIO 0 !z = return z
sumIO n !z = sumIO (n-1) (z+n)
{-# NOINLINE sumIO #-}

-- | Should /not/ unbox 'h'.
-- We won't with `lubBoxity = boxedWins`.
-- We will  with `lubBoxity = unboxedWins`.
update :: Huge -> (Bool, Huge)
update h@(Huge{f1=True}) = (False, h{f1=False})
update h                 = (True,  h)
{-# NOINLINE update #-}

-- | Should /not/ unbox 'h'.
-- We won't with `lubBoxity = boxedWins`.
-- We will  with `lubBoxity = unboxedWins`.
guarded :: (Huge -> Bool) -> Huge -> Bool
guarded g h | f1 h      = True
            | otherwise = g h
{-# NOINLINE guarded #-}

-- | Should /not/ unbox 'h'.
-- We won't with `lubBoxity = boxedWins`.
-- We will  with `lubBoxity = unboxedWins`.
--
-- This example also demonstrates the usefulness of carrying a Boxity in Poly.
-- Most absent sub-demands here should be considered Boxed (and of course we
-- also need Unboxed absent Poly). See Note [Boxity in Poly].
absent :: Huge -> Int
absent h = if f1 h || f2 h then g h else 2
  where
    g :: a -> Int
    g a = a `seq` f a True
    {-# NOINLINE g #-}
    f :: a -> Bool -> Int
    f _ True = 1
    f a False = a `seq` 2
    {-# NOINLINE f #-}
{-# NOINLINE absent #-}
