{-# OPTIONS_GHC -Wall-missed-specialisations -O2 #-}

import T16282A

import Data.Map (Map)
-- The purpose of this test is simple to trigger
-- a missed specialization and check if GHC puts
-- out a warning.
-- It used to fail to specialize on the show instance
-- for Data.Map, now that we enable -fdicts-strict by default
-- the worker for these no longer take a dictionary (having been
-- WWed). So instead we force it to fail to specialize on myMapM_

-- If someone improves the specializer so that
-- GHC no longer misses the specialization below,
-- then this test will fail, as it expects a warning
-- to be issued.
-- Another reason this could fail is due to spelling:
-- the test checks for the "specialisation" spelling,
-- but due to changes in how the warnings are listed in DynFalgs.hs
-- the compiler may spit out the "specialization" spelling.
main :: IO ()
main = do
  let m = [] :: [MyMap Double]
  myMapM_ print m
