module T21848( wombat, main)  where

wombat :: Show b => Int -> b -> String
wombat a b | a>0       = wombat (a-1) b
           | otherwise = show a ++ wombat a b

class C a where
    meth :: Show b => a -> b -> String
    dummy :: a -> () -- Force a datatype dictionary representation

instance C Int where
    meth = wombat
    dummy _ = ()

class C a => D a where
    op :: a -> a

instance D Int where
    op x = x

f :: (D a, Show b) => a -> b -> String
{-# INLINABLE[0] f #-}
f a b = meth a b ++ "!" ++ meth a b

main = putStrLn (f (42::Int) (True::Bool))
