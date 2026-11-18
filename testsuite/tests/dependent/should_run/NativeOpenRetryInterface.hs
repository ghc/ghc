{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications #-}
module Main (main) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (charVal)
import NativeOpenRetryInterfaceProducer (upper)

accepted :: Proxy 'A'
accepted = upper (Proxy @'a')

main :: IO ()
main = print (charVal accepted)
