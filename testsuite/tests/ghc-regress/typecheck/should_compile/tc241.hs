{-# OPTIONS_GHC -XGADTs -XRankNTypes -O1 #-}
-- Trac #2018

module Bug1 where

 data A a where
   MkA :: A ()

 class C w where
   f :: forall a . w a -> Maybe a

 instance C A where
   f MkA  = Just ()
