module T21128a where

indexError :: Show a => a -> a -> a ->  b
indexError a b c = error (show a ++ show b ++ show c)
{-# NOINLINE indexError #-}

index :: Int -> Int -> Int -> Int
index l u i
  | l <= i && i < u = i-l
  | otherwise       = indexError l u i
{-# INLINE index #-}
