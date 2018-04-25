{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, UndecidableInstances #-}

type family Foo a = r | r -> a where
        Foo Int = Char
        Foo Integer = String

type family Bar a = r | r -> a where
        Bar Char = Double
        Bar String = Float

type family Baz a = r | r -> a where
        Baz x = Bar (Foo x)
