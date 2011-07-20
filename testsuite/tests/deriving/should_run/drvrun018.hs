
-- Test Show on unboxed types

module Main where

data Foo = Int `MkFoo` Int deriving( Read, Show )

main = do { print (MkFoo 4 5)
	  ; print (read "3 `MkFoo` 5" :: Foo) }
