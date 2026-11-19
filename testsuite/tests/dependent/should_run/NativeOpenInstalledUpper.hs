{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications #-}
module Main (main) where

import Data.Proxy (Proxy(..))
import UnicodeExport (p3)

main :: IO ()
main = print (map fromEnum
    [p3 (Proxy @'x'), p3 (Proxy @'é'), p3 (Proxy @'!'), p3 (Proxy @'\x1e943')])
