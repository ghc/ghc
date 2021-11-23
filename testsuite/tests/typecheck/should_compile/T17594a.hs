module T17594a where

const'' :: a -> b -> a
const'' @a x _ = x

pair :: forall a. a -> (a, a)
pair @a x = (x :: a, x :: a)

id' :: a -> a
id' @a x = x

const' :: a -> b -> a
const' @a x _ = x
