module T15226b where

import Control.Exception

data StrictPair a b = MkStrictPair !a !b

testFun :: a -> b -> IO (StrictPair a b)
testFun x y = do
  x' <- evaluate x
  evaluate (MkStrictPair x' y)
  -- tag inference should not insert an eval for x' in making the strict pair
