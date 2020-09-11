module GHC.HsToCore.PmCheck.Types where

data Nabla

data Nablas--  = MkNablas (Bag Nabla)

initNablas :: Nablas
