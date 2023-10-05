module T22742 where

import GHC.Exts (TYPE)

data T (a :: TYPE r) = MkT

f :: T @(f a b) () -> ()
f MkT = ()
