{-# -fglasgow-exts #-}

-- !!! Multi-param type classes test: ambiguity bug

module ShouldFail where

class Collection c a where
    empty :: c a
    add :: a -> c a -> c a
    isempty :: c a -> Bool

singleton x = add x empty

q x = isempty (singleton x)


