{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module T25083_B where

import Control.Concurrent
import Language.Haskell.TH

tb :: Integer
tb = $(runIO (threadDelay 100000) >> [| 42 |])
