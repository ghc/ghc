{-# OPTIONS -fglasgow-exts #-}

-- !!! Mutually recursive kind inference

module ShouldCompile where

class Lookup c k a where
    lookupAll :: Sequence seq a => c -> k -> seq a

class Lookup (s a) Int a => Sequence s a where
    foo :: s a

