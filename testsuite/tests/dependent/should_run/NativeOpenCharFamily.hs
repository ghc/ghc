{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications #-}
module Main (main) where

import CharacterExport (p3)
import Data.Proxy (Proxy(..))

main :: IO ()
main = print (map fromEnum [p3 (Proxy @'x'), p3 (Proxy @'é'), p3 (Proxy @'!')])
