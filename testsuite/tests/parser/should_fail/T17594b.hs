module T17594b where

foo :: forall a. a -> a
foo @a =
  case a of
    @a -> id
