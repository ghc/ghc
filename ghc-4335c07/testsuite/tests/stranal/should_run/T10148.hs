{-# LANGUAGE BangPatterns #-}
module Main where

import Debug.Trace

data Machine = Machine (Int -> Machine) Int

main :: IO ()
main = (go 7 $ Machine (gstep (Array 99)) 8) `seq` return ()
  where
    go :: Int -> Machine -> Int
    go 0 (Machine _ done) = done
    go nq (Machine step _) = go (nq-1) $ step 0

gstep :: Array Int -> Int -> Machine
gstep m x = Machine (gstep m') (mindexA m)
  where
    !m' = adjustA x m

data Array a = Array a

adjustA :: (Show a) => Int ->  Array a -> Array a
adjustA i (Array t)
  | i < 0 = undefined i -- not just undefined!
  | otherwise = Array $ trace ("adj " ++ show t) $ t

mindexA :: Array a -> a
mindexA (Array v) = v
