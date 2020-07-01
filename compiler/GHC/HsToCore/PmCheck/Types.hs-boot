module GHC.HsToCore.PmCheck.Types where

import GHC.Data.Bag

data Nabla

newtype Nablas = MkNablas (Bag Nabla)

initNablas :: Nablas
