{-# LANGUAGE Arrows #-}
module T5022 ( pIterate ) where

import Prelude hiding ( init )

returnA :: b -> b
returnA = id

------------
newtype State s a = State { unState :: [a] }

------------
pIterate :: a -> [a]
pIterate =
    proc x -> do
      rec
        as <- unState -< s
        let s = State (x:as)
      returnA -< as
