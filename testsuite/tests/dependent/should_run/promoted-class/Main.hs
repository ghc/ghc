{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where
import ChooseControl
import System.Environment (getArgs)

data Input where
  Input :: Choose a => a -> Input

observe :: Input -> Bool
observe (Input x) = case chooseFamily x of
  TaggedTrue -> True
  TaggedFalse -> False

main :: IO ()
main = getArgs >>= print . observe . Input . (== ["yes"])
