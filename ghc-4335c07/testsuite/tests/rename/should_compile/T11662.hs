{-# LANGUAGE NamedFieldPuns #-}
module T11662 where

import T11662_A (Rec (Rec))
import qualified T11662_A

g :: Rec -> Integer
g (Rec { T11662_A.f }) = f

h :: Integer -> Rec -> Rec
h f r = r { T11662_A.f }

k :: Integer -> Rec
k f = Rec { T11662_A.f }
