module T22648a where

const_inf :: () -> forall {a} {b}. a -> b -> a
const_inf _ x _ = x

const_spec :: () -> forall a b. a -> b -> a
const_spec = const_inf