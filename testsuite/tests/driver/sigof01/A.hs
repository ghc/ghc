module A where
data T = T
    deriving (Show)
x = True
y = False
mkT = T
class Foo a where
    foo :: a -> a
instance Foo Bool where
    foo = not
