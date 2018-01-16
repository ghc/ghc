module T10781 where
{- ghc-7.10.2 reported:

T10781.hs:6:5:
    Found hole ‘_name’ with type: t
    Where: ‘t’ is a rigid type variable bound by
               the inferred type of f :: t at T10781.hs:6:1
    Relevant bindings include f :: t (bound at T10781.hs:6:1)
    In the expression: Foo._name
    In an equation for ‘f’: f = Foo._name
-}
f = Foo._name
