module Constructors where


data Foo
    = Bar
    | Baz
    | Quux Foo Int

newtype Norf = Norf (Foo, [Foo], Foo)


bar, baz, quux :: Foo
bar = Bar
baz = Baz
quux = Quux quux 0


unfoo :: Foo -> Int
unfoo Bar = 0
unfoo Baz = 0
unfoo (Quux foo n) = 42 * n + unfoo foo


unnorf :: Norf -> [Foo]
unnorf (Norf (Bar, xs, Bar)) = xs
unnorf (Norf (Baz, xs, Baz)) = reverse xs
unnorf _ = undefined
