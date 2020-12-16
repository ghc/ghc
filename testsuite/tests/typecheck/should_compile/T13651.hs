{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
module T13651 where

type family F r s = f | f -> r s

type instance F (Bar h (Foo r)) (Bar h (Foo s)) = Bar h (Bar r s)

data Bar s b
data Foo a

foo :: (F cr cu ~ Bar h (Bar r u),
        F cu cs ~ Bar (Foo h) (Bar u s))
    => Bar h (Bar r u) -> Bar (Foo h) (Bar u s) -> Foo (cr -> cs)
foo = undefined

{-  Typechecking this program used to /just/ succeed in GHC 8.2,
    (see #14745 for why), but doesn't in 8.4.

[G]  F cr cu ~ Bar h (Bar r u),
     F cu cs ~ Bar (Foo h) (Bar u s))


[W] F cr cu0 ~ Bar h (Bar r u)
    --> (top-level fundeps)  cr ~ Bar h (Foo r)
                             cu0 ~ Bar h (Foo u)
        (local fundeps)      cu ~ cu0

[W] F cu0 cs ~ Bar (Foo h) (Bar u s)
    -->  (top-level fundeps)  cu0 ~ Bar (Foo h) (Foo u)
                              cs  ~ Bar (Foo h) (Foo s)
         (local fundeps)      cu0 ~ cu

[W] F cr (Bar (Foo h) (Fo u)) ~ Bar h (Bar r u)

-}

{-
This gives us an error:
    • Could not deduce: F cr (Bar h (Foo u)) ~ Bar h (Bar r u)
      from the context: (F cr cu ~ Bar h (Bar r u),
                         F cu cs ~ Bar (Foo h) (Bar u s))
        bound by the type signature for:
                   foo :: forall cr cu h r u cs s.
                          (F cr cu ~ Bar h (Bar r u), F cu cs ~ Bar (Foo h) (Bar u s)) =>
                          Bar h (Bar r u) -> Bar (Foo h) (Bar u s) -> Foo (cr -> cs)
        at testsuite/tests/typecheck/should_compile/T13651.hs:(11,8)-(13,65)
      Expected: forall cr cu h r u cs s.
                (F cr cu ~ Bar h (Bar r u), F cu cs ~ Bar (Foo h) (Bar u s)) =>
                Bar h (Bar r u) -> Bar (Foo h) (Bar u s) -> Foo (cr -> cs)
        Actual: forall cr cu h r u cs s.
                (F cr cu ~ Bar h (Bar r u), F cu cs ~ Bar (Foo h) (Bar u s)) =>
                Bar h (Bar r u) -> Bar (Foo h) (Bar u s) -> Foo (cr -> cs)
    • In the ambiguity check for ‘foo’
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      In the type signature:
        foo :: (F cr cu ~ Bar h (Bar r u),
                F cu cs ~ Bar (Foo h) (Bar u s)) =>
               Bar h (Bar r u) -> Bar (Foo h) (Bar u s) -> Foo (cr -> cs)

despite the fact that we have:

  F cr cu ~ Bar h (Bar r u)                      (1)
    by assumption

  F cu cs ~ Bar (Foo h) (Bar u s)                (2)
    by assumption

  cr ~ Bar h (Foo r)
    by injectivity in the first argument of F
    applied to (1)

  cu ~ Bar h (Foo u)
    by injectivity in the second argument of F
    applied to (1)

  cs ~ Bar (Foo h) (Foo s)
    by injectivity in the second argument of F
    applied to (2)

So none of the variables are genuinely ambiguous.

See T13651a.hs for an example in which they are slightly
better determined, and instead GHC now reports a warning.
-}
