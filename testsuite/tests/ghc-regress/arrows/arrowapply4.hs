{-# OPTIONS -farrows #-}

module ShouldCompile where

-- example from Sebastian Boldt <Sebastian.Boldt@arcor.de>:
--	(f -< a) b  ===  f -< (a,b)

import Control.Arrow

mshowA :: (Arrow a, Show b) => a (b, String) String
mshowA = proc (x,s) -> returnA -< s ++ show x ++ s

f :: Arrow a => a Int String
f = proc x -> (mshowA -< x) "***"

g :: ArrowApply a => a Int String
g = proc x -> (mshowA -<< x) "***"
