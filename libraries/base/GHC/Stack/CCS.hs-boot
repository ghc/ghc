{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Stack.CCS where

{- Cuts the following loop:

   GHC.Exception.errorCallWithCallStackException requires
   GHC.Stack.CCS.currentCallStack, which requires
   Foreign.C (for peeking CostCentres)
   GHC.Foreign, GHC.IO.Encoding (for decoding UTF-8 strings)
   .. lots of stuff ...
   GHC.Exception
-}

import GHC.Base
import GHC.Ptr

currentCallStack :: IO [String]

getCurrentCCS :: dummy -> IO (Ptr CostCentreStack)

ccsToStrings :: Ptr CostCentreStack -> IO [String]

renderStack :: [String] -> String

data CostCentreStack
