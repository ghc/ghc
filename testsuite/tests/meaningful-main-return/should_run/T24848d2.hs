-- This program is ambiguous and emits a warning on compile,
-- Executing produces exit code 0 as its the backwards compatible behavior
{-# LANGUAGE NoMeaningfulMainReturn #-}
import System.Exit

main :: IO ExitCode
main = pure (ExitFailure 5)
