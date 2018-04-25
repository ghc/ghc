{-# LANGUAGE FlexibleInstances #-}

-- | Trusted library that unsafe plugins can use
module SafeLang09_A where

class Pos a where
    res :: a -> Bool

-- Any call to res with a list in out TCB
-- should use this method and never a more
-- specific one provided by an untrusted module
instance Pos [a] where
    res _ = True

