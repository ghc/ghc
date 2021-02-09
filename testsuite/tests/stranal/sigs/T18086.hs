{-# OPTIONS_GHC -O2 -fforce-recomp #-}
module T18086 where

import GHC.Stack
import GHC.Utils.Panic.Plain
import Control.Exception
import System.IO.Unsafe

-- Should have strictness signature <L,U>x, emphasis on the exceptional
-- divergence result.
-- CPR should still be b, as per Note [Bottom CPR iff Dead-Ending Divergence].
m :: IO ()
m = do
  putStrLn "foo"
  error "bar"

-- Ditto, just in a more complex scenario (the original reproducer of #18086)
panic :: String -> a
panic x = unsafeDupablePerformIO $ do
  stack <- ccsToStrings =<< getCurrentCCS x
  if null stack
  then throw (PlainPanic x)
  else throw (PlainPanic (x ++ '\n' : renderStack stack))

