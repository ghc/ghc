{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
module T12144 where

class C (a :: * -> *)
data T a = MkT (a -> Int) deriving C
