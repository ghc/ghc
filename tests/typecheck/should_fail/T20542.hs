{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module T20542 where

class Foo a b where
    foo :: a -> b

instance {-# OVERLAPPABLE #-} Show a => Foo a String where
    foo = show

instance {-# OVERLAPPING #-} Read b => Foo String b where
    foo = read

bar :: String -> String
bar = foo
