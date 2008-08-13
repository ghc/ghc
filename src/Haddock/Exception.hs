--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


{-# LANGUAGE DeriveDataTypeable #-}


module Haddock.Exception (
  HaddockException,
  throwE
) where


import Data.Typeable
import Control.Exception


data HaddockException = HaddockException String deriving Typeable


instance Show HaddockException where
  show (HaddockException str) = str


#if __GLASGOW_HASKELL__ >= 609
instance Exception HaddockException
throwE str = throw (HaddockException str)
#else
throwE str = throwDyn (HaddockException str)
#endif
