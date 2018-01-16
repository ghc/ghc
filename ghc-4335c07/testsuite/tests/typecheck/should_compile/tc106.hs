{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- !!! Mutually recursive kind inference
-- Exposes a bug in 4.08 (fixed in 4.08 pl1)

module ShouldCompile where

-- This pair will tickle the bug
class Lookup c k a where
    lookupAll :: Sequence seq a => c -> k -> seq a

class Lookup (s a) Int a => Sequence s a where
    foo :: s a


-- This decl will tickle it all by itself
class Matrix a e where
   amap2  :: (Matrix a d) =>
             (e -> d -> e) -> a ix e -> a ix d -> a ix e

