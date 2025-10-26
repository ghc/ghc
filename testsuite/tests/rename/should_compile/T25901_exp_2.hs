{-# LANGUAGE ExplicitNamespaces #-}

module T25901_exp_2 where

import T25901_exp_2_helper (data m, data E, data f, data f2,
                            data D, data D2, data v)

g :: Int -> ()
g x = let e = E x x
          _ = f e
          _ = f2 e
          _ = m @Int
          _ = v
      in ()
