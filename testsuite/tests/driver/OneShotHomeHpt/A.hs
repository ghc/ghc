module Main where

-- A only imports B.  C is demand-loaded while compiling A, and its instances,
-- family instances and rules must be visible nonetheless.
import B

main :: IO ()
main = do
  print t             -- Show T instance from C
  print (k + 1)       -- F T ~ Int from C
  print (useG 5)      -- RULE "g/42" from C fires after inlining useG
