{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           GHC.Stack

-- | Some useless pattern synonym that groups a value with the call stack using
-- view patterns. In the real code base where I'm using this this pattern
-- synonym generates part of an abstract syntax tree instead.
pattern Annotated :: HasCallStack => (CallStack, a) -> a
pattern Annotated x <- (addCallStack -> x)
  where
    Annotated (_, x) = x

-- | Used in 'SomeSynonym' to pair a value with the current call stack, since
-- you cannot add the 'HasCallStack' constraint to a lambda (in the real use
-- case we would be calling a function that does something with the call stack
-- here).
addCallStack :: HasCallStack => a -> (CallStack, a)
addCallStack x = (callStack, x)

someAnnotatedValue :: (CallStack, Int)
someAnnotatedValue = let Annotated annotated = 10 in annotated


main :: IO ()
main = do
    let (stack, _) = someAnnotatedValue
    putStrLn "No lines from within 'someAnnotatedValue' (i.e. line 24) will show up here:"
    putStrLn $ prettyCallStack stack
