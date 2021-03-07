{-# LANGUAGE Haskell2010 #-}
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


unnorf' :: Norf -> Int
unnorf' x@(Norf (f1@(Quux _ n), _, f2@(Quux f3 _))) =
    x' + n * unfoo f1 + aux f3
  where
    aux fx = unfoo f2 * unfoo fx * unfoo f3
    x' = sum . map unfoo . unnorf $ x
