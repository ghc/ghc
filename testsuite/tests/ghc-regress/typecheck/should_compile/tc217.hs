{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -w #-}

module ShouldCompile where


import Control.Monad.Reader

instance Eq (a -> b) where
     _ == _ = error "whoops"

instance Show (a -> b) where
     show = const "<fun>"

-- This is the exmaple from Trac #179
foo x = show (\_ -> True)

-- This is the example from Trac #963
instance (Num a, Monad m, Eq (m a), Show (m a)) => Num (m a) where
test = 1 True
