{-# LANGUAGE MagicHash, UnboxedTuples #-}

module T502 where

-- As per #502, the following type error message should correctly
-- display the unboxed tuple type.
bar :: Int
bar = snd foo
  where foo :: (# Int, Int #)
        foo = undefined
