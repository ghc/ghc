module UnamedConstructorFields where

data A = A
data B = B
data C = C

-- Haddock on 'A' prevents the haddock on 'C' from being applied to 'MkFoo'
data Foo = MkFoo A -- ^ 'A' has a comment
                 B -- This doesn't
                 C -- ^ 'C' has a comment
