{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module Main where

newtype Mu f = Mu (f (Mu f)) 
     
type family Id m
type instance Id m = m

instance Show (Id (f (Mu f))) => Show (Mu f) where
    show (Mu f) = show f


showMu :: Mu (Either ()) -> String
showMu = show

item :: Mu (Either ())
item = Mu (Right (Mu (Left ())))

main = print (showMu item)
