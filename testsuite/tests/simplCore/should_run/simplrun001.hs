-- !!! Test filter fusion

-- In GHC 4.06, the filterFB rule was back to front, which
-- made this program hit the "error foo" case instead of
-- working fine.


module Main where

main :: IO ()
main = print (length (filter (not . foo)
                             (filter (const False) [Nothing])))
  where foo (Just x) = x
        foo _        = error "foo"
