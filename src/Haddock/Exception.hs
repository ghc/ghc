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


-- TODO: change this to test for base version instead
#if __GLASGOW_HASKELL__ >= 609 
import Control.OldException
#else
import Control.Exception
#endif


data HaddockException = HaddockException String deriving Typeable
throwE str = throwDyn (HaddockException str)


instance Show HaddockException where
  show (HaddockException str) = str
