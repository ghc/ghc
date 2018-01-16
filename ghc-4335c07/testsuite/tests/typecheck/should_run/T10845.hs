{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -dcore-lint #-}

import GHC.Stack

f1 :: (?loc :: CallStack) => CallStack
-- we can solve CallStacks in local functions from CallStacks
-- in the outer context
f1 = let y x = (?loc :: CallStack)
     in y 0

main :: IO ()
main = do putStrLn $ prettyCallStack f1
