{-# OPTIONS_GHC -fglasgow-exts #-}
-- !!! Class variable constraints on member funs
module M where
class C a where f :: Eq a => a
