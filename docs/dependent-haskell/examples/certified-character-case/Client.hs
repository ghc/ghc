{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications #-}
module Main (main) where

import Data.Proxy (Proxy(..))
import P3 (p3)

main :: IO ()
main = print (map fromEnum
  [p3 (Proxy @'x'), p3 (Proxy @'\xe9'), p3 (Proxy @'\x1e943'), p3 (Proxy @'!')])
