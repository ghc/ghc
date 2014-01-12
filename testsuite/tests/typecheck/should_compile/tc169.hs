-- This one briefly killed the new GHC 6.4

module Foo where

newtype Foo x = Foo x
-- data Foo x = Foo x -- this works

class X a where
	x :: a -> IO ()

class X a => Y a where
	y :: [a] -> IO ()

class Z z where
	z :: Y c => z c -> IO ()

instance X Char where
	x = putChar
instance X a => X (Foo a) where
	x (Foo foo) = x foo

instance Y Char where
    y cs = mapM_ x cs
        
instance Z Foo where
	z = x 

