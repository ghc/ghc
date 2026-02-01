{-# LANGUAGE QualifiedStrings #-}

import qualified Example.Length as Length

-- Test error messages containing qualified strings in expressions
main :: IO ()
main = putStrLn Length."this fails after being converted into an Int"
