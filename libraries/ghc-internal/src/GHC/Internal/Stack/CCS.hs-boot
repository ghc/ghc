{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Internal.Stack.CCS where

{- Cuts the following loop:

   GHC.Internal.Exception.errorCallWithCallStackException requires
   GHC.Internal.Stack.CCS.currentCallStack, which requires
   GHC.Internal.Foreign.C (for peeking CostCentres)
   GHC.Foreign, GHC.Internal.IO.Encoding (for decoding UTF-8 strings)
   .. lots of stuff ...
   GHC.Internal.Exception
-}

import GHC.Internal.Base

currentCallStack :: IO [String]
