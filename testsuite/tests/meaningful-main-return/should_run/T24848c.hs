-- This program exits with status code 5
{-# LANGUAGE MeaningfulMainReturn #-}
import System.Exit

main :: IO ExitCode
main = pure (ExitFailure 5)
