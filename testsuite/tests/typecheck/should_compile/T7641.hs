{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module T7641 where

data Foo b = Foo deriving Show

class ToFoo a b where
    toFoo :: a -> Foo b

instance ToFoo (c -> ()) b where
    toFoo _ = Foo

baz () = toFoo $ \_ -> ()
