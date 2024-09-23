-- This program is ambiguous, and should have a warning when compiles
{-# LANGUAGE NoMeaningfulMainReturn #-}
import System.Exit

main :: IO ExitCode
main = pure (ExitFailure 5)
