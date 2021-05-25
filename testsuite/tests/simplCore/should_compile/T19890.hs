module T19890 where

newtype Wombat a = Wombat (a->a)

foo :: Num a => Bool -> Wombat a
{-# INLINEABLE foo #-}
foo True  = foo False
foo False = Wombat (\x -> x+1)
