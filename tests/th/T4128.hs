{-# LANGUAGE TemplateHaskell #-}
module T4128 where

import Language.Haskell.TH
class C a where
data X = X
fmap return $ instanceD (cxt []) [t| C $(conT ''X) |] []
