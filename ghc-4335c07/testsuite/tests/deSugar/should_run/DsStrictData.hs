{-# LANGUAGE ScopedTypeVariables, StrictData, GADTs #-}

-- | Tests the StrictData LANGUAGE pragma.
module Main where

import qualified Control.Exception as E
import System.IO.Unsafe (unsafePerformIO)

data Strict a = S a
data Strict2 b = S2 !b
data Strict3 c where
  S3 :: c -> Strict3 c

data UStrict = US {-# UNPACK #-} Int

data Lazy d = L ~d
data Lazy2 e where
  L2 :: ~e -> Lazy2 e

main :: IO ()
main =
  do print (isBottom (S bottom))
     print (isBottom (S2 bottom))
     print (isBottom (US bottom))
     print (isBottom (S3 bottom))
     putStrLn ""
     print (not (isBottom (L bottom)))
     print (not (isBottom (L2 bottom)))
     print (not (isBottom (Just bottom))) -- sanity check

------------------------------------------------------------------------
-- Support for testing for bottom

bottom :: a
bottom = error "_|_"

isBottom :: a -> Bool
isBottom f = unsafePerformIO $
  (E.evaluate f >> return False) `E.catches`
    [ E.Handler (\(_ :: E.ArrayException)   -> return True)
    , E.Handler (\(_ :: E.ErrorCall)        -> return True)
    , E.Handler (\(_ :: E.NoMethodError)    -> return True)
    , E.Handler (\(_ :: E.NonTermination)   -> return True)
    , E.Handler (\(_ :: E.PatternMatchFail) -> return True)
    , E.Handler (\(_ :: E.RecConError)      -> return True)
    , E.Handler (\(_ :: E.RecSelError)      -> return True)
    , E.Handler (\(_ :: E.RecUpdError)      -> return True)
    ]
