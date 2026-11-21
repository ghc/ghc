{-# LANGUAGE NoExperimentalDependentTypes #-}

module Main (main) where

import NativeOffer (choose)

-- | Call the retained provider through its stripped interface.
main :: IO ()
main = print (choose True :: String, choose False :: Bool)
