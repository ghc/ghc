{-# LANGUAGE ImplicitParams #-}
module WarnDefaultedCallStack where

import GHC.Stack

intCs :: HasCallStack => Int
intCs = 0

topLevelNoWarning :: HasCallStack => Int
topLevelNoWarning = intCs

outerNoWarning :: HasCallStack => IO ()
outerNoWarning = innerNoWarning (1000::Int)
  where
    innerNoWarning = \case
      0 -> error "inner" -- gets CallStack from outerNoWarning
      n -> innerNoWarning $ n - 1

topLevelExplicitEmptyCallStackNoWarning :: IO ()
topLevelExplicitEmptyCallStackNoWarning = withEmptyCallStack $ do
  print $ intCs + localWarns
  where
    -- No enclosing CallStack, intCs warns.
    localWarns = intCs + localNoWarning

    -- Implicit parameters of type CallStack also default.
    implicitWarns :: CallStack
    implicitWarns = ?other

    localNoWarning :: HasCallStack => Int
    localNoWarning = intCs + nestedNoWarning
      where
        nestedNoWarning = intCs -- gets CallStack from localNoWarn

topLevelWarns :: IO ()
topLevelWarns = print intCs

separateTopLevelWarns :: Int
separateTopLevelWarns = topLevelNoWarning

withinDefUnderReports :: Int
withinDefUnderReports =
  -- Only one warning reported here, the other absent due to a CSE'd wanted.
  intCs + intCs
