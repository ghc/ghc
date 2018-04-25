{-# LANGUAGE DataKinds #-}
module Main (main) where

import Data.Typeable
import GHC.Types

main :: IO ()
main = do
  print (typeOf (Proxy :: Proxy 'Just))
  print (typeOf (Proxy :: Proxy (TYPE 'LiftedRep)))
  print (typeOf (Proxy :: Proxy (TYPE 'UnliftedRep)))
