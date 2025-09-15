{-# LANGUAGE TypeFamilies #-}
module T22717b where

import T22717c ()
import T22717d

p :: F (F T) -> Int
p _ = 3
