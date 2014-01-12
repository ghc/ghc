

-- !!! Check non-constructors in patterns fail tidily
-- !!! The -O made ghc 4.08 go into a loop!
-- Unfortunately the -O has to go in the Makefile

module ShouldFail where

compute :: String -> String
compute ("hd" ++ _) = "_"
