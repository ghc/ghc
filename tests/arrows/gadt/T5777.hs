{-# LANGUAGE Arrows, GADTs #-}
module T5777 where

import Control.Arrow

data Value a where
    BoolVal :: Value Bool

class ArrowInit f where
    arrif :: f b -> ()

instance ArrowInit Value where
    arrif = proc BoolVal -> returnA -< ()
    -- arrif = arr (\BoolVal -> ())
