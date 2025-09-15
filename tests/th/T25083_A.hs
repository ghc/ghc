{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module T25083_A where

import Control.Concurrent
import Language.Haskell.TH

ta :: Integer
ta =
  $(do runIO (threadDelay 100000)
       litE . integerL . toInteger . length =<< reifyInstances ''Show [])
