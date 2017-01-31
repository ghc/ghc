{-# LANGUAGE DataKinds #-}
module Main (main) where

import Data.Typeable
import GHC.Types

main :: IO ()
main = do
  print (typeOf (Proxy :: Proxy 'Just))
  print (typeOf (Proxy :: Proxy (TYPEV 'Visible 'LiftedRep)))
  print (typeOf (Proxy :: Proxy (TYPEV 'Invisible 'LiftedRep)))
  print (typeOf (Proxy :: Proxy (TYPEV 'Visible 'UnliftedRep)))
