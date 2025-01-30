{-# LANGUAGE ImplicitParams #-}

module Main where

import Data.Maybe
import Debug.Trace
import GHC.IsList
import GHC.Stack

hd (c:cs) = c
hd [] = error "urk"

what :: (HasCallStack) => Int
what =
  let cs = getCallStack callStack
   in srcLocStartCol (snd (hd cs))

main :: IO ()
main =
  let ?callStack = fromList []
   in print (what, what)
