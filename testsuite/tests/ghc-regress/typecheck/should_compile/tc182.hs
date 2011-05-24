{-# LANGUAGE DatatypeContexts, ExistentialQuantification #-}

-- Tests the "stupid theta" in pattern-matching
-- when there's an existential as well

module ShouldCompile  where

data (Show a) => Obs a = forall b. LiftObs a b

f :: Show a => Obs a -> String
f (LiftObs _ _) = "yes"
                 

