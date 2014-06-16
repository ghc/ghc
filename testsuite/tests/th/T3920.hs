{-# LANGUAGE EmptyDataDecls, TypeFamilies, TemplateHaskell #-}
module Main where

import Language.Haskell.TH

type family S :: (* -> (* -> * -> *)) -> (* -> *) -> *

$(return [])

test :: String
test = $(do
	test <- [d| 
		type family T :: (* -> (* -> * -> *)) -> (* -> *) -> * |]
        blah <- reify ''S
	return (LitE (StringL (pprint test ++ "\n" ++ pprint blah))))

main = putStrLn test
