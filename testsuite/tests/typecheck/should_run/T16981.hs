{-# LANGUAGE StaticPointers #-}
-- Main.hs
module Main where

import GHC.StaticPtr

class UniqueHash a where
  hash :: a -> String

{-# NOINLINE unCacheable #-}
unCacheable :: Cacheable a -> a
unCacheable (CExplicit _ a)  = a

data Cacheable a = CExplicit String a

instance UniqueHash (StaticPtr  a) where
  hash ptr = show $ staticKey ptr

instance IsStatic Cacheable where
  fromStaticPtr ptr = CExplicit h a where
    h = hash ptr
    a = deRefStaticPtr ptr

{-# NOINLINE splitInjectedAsWindowed #-}
splitInjectedAsWindowed :: Int
--  WindowedDataset
splitInjectedAsWindowed = unCacheable $ static 1


main :: IO ()
main = print splitInjectedAsWindowed
