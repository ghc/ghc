{-# LANGUAGE ConstraintKinds #-}       -- For 'C'
{-# LANGUAGE MultiParamTypeClasses #-} -- For nullary 'Trivial' class
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Main where

import qualified GHC.Stack as Ghc

class Trivial where
instance Trivial where

type C = (Ghc.HasCallStack, Trivial)

-- | Print the functions on the call stack.
callStack :: C => IO ()
callStack = print $ map fst (Ghc.getCallStack Ghc.callStack)

f :: C => a -> IO ()
f x = callStack

type C2 =  (?x::Int, ?y::Int)

h1 :: C2 => Int -> IO ()
h1 v = print (?x+v)

h2 :: C2 => Int -> IO ()
h2 v = let ?x = 0 in h1 v

main = do { let { ?x = 3; ?y = 4 }  in h2 4
            -- Should print 4+0 = 4

          ; f "ugh"
            -- Should print @["callStack", "f"]@.
          }
