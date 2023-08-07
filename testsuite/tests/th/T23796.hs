{-# LANGUAGE TemplateHaskell #-}
module T23796 where

good :: (forall a. a -> a) -> b -> b
good = \g x -> g x

bad :: (forall a. a -> a) -> b -> b
bad = $([| \g x -> g x |])
