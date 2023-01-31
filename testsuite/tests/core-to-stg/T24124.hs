module T24124 where

import Control.Exception

data StrictPair a b = MkStrictPair !a !b

testFun :: a -> b -> IO (StrictPair a b)
testFun x y = do
  x' <- evaluate x
  evaluate (MkStrictPair x' y)
  -- This MkStrictPair application should not result in a thunk at run-time
