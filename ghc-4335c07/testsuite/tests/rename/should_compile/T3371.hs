{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}

-- Should warn about the unused 'a', but not about the unused 'c'

module T3371(bar) where

data Foo = Foo { a,b,c :: Int } deriving(Eq)

bar Foo{ a = a, ..} = print b
