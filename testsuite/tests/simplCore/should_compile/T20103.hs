module T20103 where

import GHC.Stack

foo :: HasCallStack => Int -> Int
foo 0 = length . fst . head $ getCallStack callStack
foo n = foo (n-1)

