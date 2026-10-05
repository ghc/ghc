{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module C where

import splice Language.Haskell.TH
import splice "a" Util
import quote "b" Util
import "b" B.Normal
import splice B.Splice
import qualified Data.Map as Map

c :: Int
c = $(quoted) + normal
