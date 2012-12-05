module T7164 where

class Foo m where
    herp :: (a -> a) -> m b -> m b
    derp :: m a

derp :: Int
derp = 123
