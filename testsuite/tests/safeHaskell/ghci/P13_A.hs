{-# LANGUAGE OverlappingInstances, FlexibleInstances #-}
module P13_A where

class Pos a where { res :: a -> Bool }

instance Pos [a] where { res _ = True }

instance Pos Char where { res _ = True }

