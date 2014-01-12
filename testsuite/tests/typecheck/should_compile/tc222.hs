{-# LANGUAGE ImplicitParams, RankNTypes #-}

-- Tests impredivative polymorphism with left-to-right
-- flow information; see the uses of "$"

module TestIP where

import Control.Monad.ST
import Data.STRef

-- Here's a use of runST with ($)
foo = runST $ (do { v <- newSTRef 0; readSTRef v })

-- Here's a use of implicit parameters with ($)

type PPDoc = (?env :: Int) => Char -> Char

f :: PPDoc -> PPDoc
f c = g $ c

-- Fully annotated version of f, as compiled by GHC 6.4.2
--
-- f ?env c = $ (C->C) (C->C) 
--		(\(x:C->C). g ?env (\?env. x))
--		(c ?env)
--
-- The subsumption test needed from the call to $ is this:
--	?env => (?env => C -> C) -> C -> C   <=  a->b
--	(?env => C -> C) -> C -> C   <=  a->b
--	(a) C->C <= b
--	(b) a <= (?env => C -> C)
-- And perhaps surprisingly (b) succeeds!

g :: PPDoc -> PPDoc
g d = d



