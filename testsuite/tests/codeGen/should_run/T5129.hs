{-# LANGUAGE DeriveDataTypeable #-}
import Control.Exception as E
import Data.Typeable

throwIfNegative :: Int -> String
throwIfNegative n | n < 0     = error "negative"
                  | otherwise = "no worries"
{-# NOINLINE throwIfNegative #-}

data HUnitFailure = HUnitFailure String deriving (Show,Typeable)
instance Exception HUnitFailure

assertFailure msg = E.throw (HUnitFailure msg)

case_negative =
    handleJust errorCalls (const $ return ()) $ do
        evaluate $ throwIfNegative (-1)
        assertFailure "must throw when given a negative number"
  where errorCalls (ErrorCall _) = Just ()

main = case_negative
