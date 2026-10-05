{-# LANGUAGE TemplateHaskell #-}
module A.TH where

import Language.Haskell.TH
import A.Base

liftBase :: Q Exp
liftBase = [| base |]
