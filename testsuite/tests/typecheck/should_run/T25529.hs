{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

module Main where

import GHC.Stack (HasCallStack, CallStack, SrcLoc(srcLocStartLine, srcLocStartCol), callStack, getCallStack)

main :: IO ()
main =
  let ?myImplicitParam = ()
   in run action

type MyConstraints = (HasCallStack, ?myImplicitParam :: ())

action :: MyConstraints => IO ()
action = run $ pure ()

-- | Print the current call stack and then run an action.
run ::
  MyConstraints =>
  IO a ->
  IO a
run action = do
  let prettyCallStack = unlines $ map prettyCallStackEntry $ getCallStack callStack
      prettyCallStackEntry (name, loc) =
        name
        <> ", called at "
        <> show (srcLocStartLine loc)
        <> ":"
        <> show (srcLocStartCol loc)
  putStrLn "============================================================"
  putStrLn prettyCallStack
  action
