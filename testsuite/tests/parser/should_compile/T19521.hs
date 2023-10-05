{-# LANGUAGE OverloadedRecordDot #-}

module T19521 where

data Foo =
  Foo {
    val :: Int,
    fun :: Int -> Int
  }

apply :: Foo -> Int
apply foo = foo.fun foo.val
