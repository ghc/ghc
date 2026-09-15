{-# LANGUAGE Arrows #-}

-- Regression test for lazy matching of an arrow 'rec' feedback tuple.
--
-- Ensures that the boxing/unboxing logic of Note [Boxing big tuple elements]
-- in GHC.HsToCore.Utils does not force the feedback tuple.
module Main (main) where

import Control.Arrow

f :: Int -> Int
f = proc x -> do
        rec a <- returnA -< b + 1
            b <- returnA -< c + 1
            c <- returnA -< x
        returnA -< a

main :: IO ()
main = print (f 5)
