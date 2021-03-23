{-# OPTIONS_GHC -O2 -fforce-recomp #-}
module T18086 where

import GHC.Stack
import GHC.Utils.Panic.Plain
import Control.Exception
import System.IO.Unsafe

-- Should have strictness signature <L>x, emphasis on the exceptional
-- divergence result.
m :: IO ()
m = do
  putStrLn "foo"
  error "bar"

-- Ditto, just in a more complex scenario (the original reproducer of #18086)
-- NB: This depends on the strictness signature of GHC.Magic.runRW#, which will
-- be topSig if compiled with optimisation as in the quick flavour. In this case
-- we'll see <L>, without x.
panic :: String -> a
panic x = unsafeDupablePerformIO $ do
  stack <- ccsToStrings =<< getCurrentCCS x
  if null stack
  then throw (PlainPanic x)
  else throw (PlainPanic (x ++ '\n' : renderStack stack))

