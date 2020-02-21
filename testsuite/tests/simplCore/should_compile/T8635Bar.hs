{-# OPTIONS_GHC -O2 -ddump-rule-firings #-}
module T8635Bar where
import T8635Foo
bar :: Int -> Bool
bar x = x == 72
