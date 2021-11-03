module T17594a where

id1 :: forall a. a -> a
id1 @a x = x

id2 :: forall a. a -> a
id2 @_ x = x

id3 :: forall a. a -> a
id3 @a (x :: a) = x

const' :: forall a. a -> forall b. b -> a
const' @a x @b y = x
