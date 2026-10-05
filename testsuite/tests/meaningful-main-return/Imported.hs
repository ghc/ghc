module Imported where

import System.Exit

main :: IO ExitCode
main = pure (ExitFailure 14)

mainInvalid :: IO Int
mainInvalid = pure 42
