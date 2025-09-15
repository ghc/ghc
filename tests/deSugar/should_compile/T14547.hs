{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module T14547 where

class Foo f where
        type It f
        foo :: [It f] -> f

data List a = Empty | a :! List a deriving Show

instance Foo (List a) where
        type It (List a) = a
        foo [] = Empty
        foo (x : xs) = x :! foo xs
