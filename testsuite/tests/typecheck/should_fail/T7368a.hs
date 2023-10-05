{-# LANGUAGE Rank2Types, KindSignatures #-}
module T7368 where

newtype Bad w = Bad (forall a. (w a -> a))
-- Bad :: forall w. (forall a. w a -> a) -> Bad w

fun :: forall (f :: * -> *). f (Bad f) -> Bool
fun (Bad x) = True 

{-  f (Bad f) ~ Bad w
-->
    f ~ Bad
    Bad f ~ w
-}
