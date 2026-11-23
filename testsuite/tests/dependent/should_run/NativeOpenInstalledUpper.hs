{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications, TypeOperators #-}
module Main (main) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:) (Refl))
import UnicodeExport (Lower, Title, p3)

main :: IO ()
main = print (map fromEnum
    [p3 (Proxy @'x'), p3 (Proxy @'é'), p3 (Proxy @'!'), p3 (Proxy @'\x1e943')])

titleCase :: Title '\x1c6' :~: '\x1c5'
titleCase = Refl

lowerCase :: Lower '\x1c4' :~: '\x1c6'
lowerCase = Refl
