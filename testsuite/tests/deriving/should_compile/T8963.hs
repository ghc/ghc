{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module T8963 where

class C c where
   data F c r

instance C Int where
   newtype F Int r = F (IO r) deriving (Functor)
