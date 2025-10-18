{-# LANGUAGE QualifiedStrings #-}

import qualified Example.Length as Length

-- Test error messages containing qualified strings in patterns
main :: IO ()
main =
  case "" of
    Length."this fails after being converted into an Int" -> pure ()
    _ -> pure ()
