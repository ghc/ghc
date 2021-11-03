module T17594 where

id1 :: forall a. a -> a
id1 @a x = x

id2 :: forall a. a -> a
id2 @_ x = x

id3 :: forall a. a -> a
id3 @a (x :: a) = x
