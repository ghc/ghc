{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances, AllowAmbiguousTypes #-}

module T8392 where

class Fun f a b where
    fun :: f -> a -> b

instance (b ~ Int, a ~ Int) => Fun F a b
    where fun _ = (+1)

data F = F

data Compose a b = Compose a b

-- ghc-7.6 version
instance (Fun f b c, Fun g a b) => Fun (Compose f g) a c where
    fun (Compose f g) a = fun f (fun g a :: b)