{-# LANGUAGE DatatypeContexts, ExistentialQuantification #-}

-- Tests the "stupid theta" in pattern-matching
-- when there's an existential as well

module ShouldCompile  where

data (Show a) => Obs a = forall b. LiftObs a b

f :: Obs a -> String	-- Needs a (Show a) context
f (LiftObs _ _) = "yes"
