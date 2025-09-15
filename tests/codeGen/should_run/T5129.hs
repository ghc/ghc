{-# LANGUAGE DeriveDataTypeable #-}
import Control.Exception as E
import Data.Typeable

throwIfNegative :: Int -> String
throwIfNegative n | n < 0     = error "negative"
                  | otherwise = "no worries"
{-# NOINLINE throwIfNegative #-}

data HUnitFailure = HUnitFailure String deriving (Show,Typeable)
instance Exception HUnitFailure

assertFailure :: String -> a -- Not an IO function!
assertFailure msg = E.throw (HUnitFailure msg)

main :: IO ()
main =
    handleJust errorCalls (const (return ())) (do
        evaluate (throwIfNegative (-1)) -- Pure expression evaluated in IO
        assertFailure "must throw when given a negative number")
  where errorCalls (ErrorCall _) = Just ()

