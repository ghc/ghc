{-# OPTIONS -fglasgow-exts #-}

module Main where

import Data.Typeable

newtype Y e = Y { unY :: (e (Y e)) } 

instance Typeable1 e => Typeable (Y e) where
   typeOf _ = mkTyConApp yTc [typeOf1 (undefined :: e ())]

yTc :: TyCon
yTc = mkTyCon "Main.Y"

main = print (typeOf (undefined :: Y []))
