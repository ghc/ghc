module Main where

-- base
import Data.List
  ( intersect )
import System.Exit
  ( exitFailure, exitSuccess )

-- ghc
import GHC.Driver.Flags
  ( standardWarnings, minusWcompatOpts )

--------------------------------------------------------------------------------

-- Test that there are no warning flags in both the -Wcompat and -Wdefault
-- warning groups.

main :: IO ()
main = do
  case minusWcompatOpts `intersect` standardWarnings of
    [] -> exitSuccess
    badWarnings -> do
      putStrLn $ unlines $
        "The following warning flags are in both -Wcompat and -Wdefault:"
        : map (("  - " ++) . show) badWarnings
      exitFailure
