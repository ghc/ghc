{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- Strangely, this program does not elicit an error message
-- in GHC 5.03.  I don't know why. It fails correctly in
-- 5.04


module ShouldFail where

class Eq ce => Collects e ce | ce -> e where
    empty :: ce
    empty = error("empty")

data Stupid = Stupid -- without equality

instance Collects Bool Stupid where
