module T18355 where

import GHC.Exts

-- I expect the simplified Core to have an eta-expaned
-- defn of f, with a OneShot on the final lambda-binder
f x b = case b of
          True -> oneShot (\y -> x+y)
          False -> \y -> x-y
