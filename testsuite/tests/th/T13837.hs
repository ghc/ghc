{-# LANGUAGE TemplateHaskell #-}
module T13837 where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

test_local_tyfam_expansion :: String
test_local_tyfam_expansion =
  $(do fam_name <- newName "Fam"
       stringE . show =<< qReifyInstances fam_name [])
