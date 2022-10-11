{-# LANGUAGE LambdaCase #-}

module TestUtils where

import GHC.Exts.DecodeStack
import GHC.Stack (HasCallStack)

assertEqual :: (HasCallStack, Monad m, Show a, Eq a) => a -> a -> m ()
assertEqual a b
  | a /= b = error (show a ++ " /= " ++ show b)
  | otherwise = pure ()

assertThat :: (HasCallStack, Monad m) => String -> (a -> Bool) -> a -> m ()
assertThat s f a = if f a then pure () else error s

assertStackInvariants :: (HasCallStack, Monad m) => [StackFrame] -> m ()
assertStackInvariants decodedStack =
  assertThat
    "Last frame is stop frame"
    ( \case
        StopFrame -> True
        _ -> False
    )
    (last decodedStack)
