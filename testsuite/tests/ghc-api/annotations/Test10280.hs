{-# LANGUAGE TupleSections #-}
module Test10280 where

foo2 = atomicModifyIORef ciTokens ((,()) . f)
