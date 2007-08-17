module Haddock.Exception (
  HaddockException,
  throwE
) where


import Data.Typeable
import Control.Exception


data HaddockException = HaddockException String deriving Typeable
throwE str = throwDyn (HaddockException str)


instance Show HaddockException where
  show (HaddockException str) = str
