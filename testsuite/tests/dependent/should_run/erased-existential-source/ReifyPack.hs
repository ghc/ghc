{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module ReifyPack where

import GHC.Exists
import Language.Haskell.TH (Info(DataConI), recover, reify)

data Ordinary = Ordinary
$(pure [])

protected :: Bool
protected = $(do
  hidden <- recover (pure True) (False <$ reify 'Pack)
  constructor <- reify 'Ordinary
  case (hidden, constructor) of
    (True, DataConI {}) -> [| True |]
    _ -> fail "Pack reification exposed its implementation marker")
