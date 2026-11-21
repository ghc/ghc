{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where
import ChooseEvidence
import System.Environment (getArgs)

data Inputs where
    Inputs :: (Choose b, Choose a) => a -> b -> Inputs

observe :: Inputs -> (Bool, Bool)
observe (Inputs x y) = chooseBoth x y

main :: IO ()
main = do
    yes <- (== ["yes"]) <$> getArgs
    print (observe (Inputs yes (Inverted yes)))
