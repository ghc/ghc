module T17594b where

f :: forall a. a -> a
f @x =
  case x of
    @a -> id
