{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

module T14380 where

data Foo = Foo [Int]
pattern Bar :: Foo
pattern Bar = Foo []
