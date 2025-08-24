{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE NoDataKinds #-}
module T26318 where

class C1 l
instance C1 (x : xs)

class C2 l
instance C2 (x ': xs)

class C3 l
instance C3 ((:) x xs)

class C4 l
instance C4 ('(:) x xs)
