
module Main where

import Language.Haskell.TH

$( do let h x = x
	  foo = [| \x -> $(h [| x |]) |]

      [d| baz = $foo |]
 )

main = print (baz "Hello")

