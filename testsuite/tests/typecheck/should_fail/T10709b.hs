module T10709b where

import GHC.IO
import Control.Monad

x4 = (replicateM 2 . mask) (\_ -> return ())
x5 = (replicateM 2 . mask) (\x -> undefined x)
x6 = (replicateM 2 . mask) (id (\_ -> undefined))
x7 = (replicateM 2 . mask) (const undefined)
x8 = (replicateM 2 . mask) ((\x -> undefined x) :: a -> b)
