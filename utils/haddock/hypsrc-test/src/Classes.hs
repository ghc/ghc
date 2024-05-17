{-# LANGUAGE Haskell2010 #-}
module Classes where


class Foo a where
    bar :: a -> Int
    baz :: Int -> (a, a)

instance Foo Int where
    bar = id
    baz x = (x, x)

instance Foo [a] where
    bar = length
    baz _ = ([], [])


class Foo a => Foo' a where
    quux :: (a, a) -> a
    quux (x, y) = norf [x, y]

    norf :: [a] -> a
    norf = quux . baz . sum . map bar

instance Foo' Int where
    norf = sum

instance Foo' [a] where
    quux = uncurry (++)


class Plugh p where
    plugh :: p a a -> p b b -> p (a -> b) (b -> a)

instance Plugh Either where
    plugh (Left a) _ = Right $ const a
    plugh (Right a) _ = Right $ const a
    plugh _ (Left b) = Left $ const b
    plugh _ (Right b) = Left $ const b
