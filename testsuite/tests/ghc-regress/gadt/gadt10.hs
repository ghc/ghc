{-# OPTIONS -fglasgow-exts #-}

module ShouldFail where

-- Kind error
data RInt a where R :: RInt
