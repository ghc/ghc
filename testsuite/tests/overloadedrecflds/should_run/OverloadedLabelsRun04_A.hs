{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module OverloadedLabelsRun04_A where

import GHC.OverloadedLabels
import Language.Haskell.TH

instance IsLabel x (Q [Dec]) where
  fromLabel _ = [d| main = putStrLn "Ok" |]
