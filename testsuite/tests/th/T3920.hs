{-# LANGUAGE EmptyDataDecls, TypeFamilies, TemplateHaskell #-}
module Main where

import Language.Haskell.TH hiding (Type)
import Data.Kind (Type)

type family S :: (Type -> (Type -> Type -> Type))
              -> (Type -> Type) -> Type

$(return [])

test :: String
test = $(do
        test <- [d|
                type family T :: (Type -> (Type -> Type -> Type))
                              -> (Type -> Type) -> Type |]
        blah <- reify ''S
        return (LitE (StringL (pprint test ++ "\n" ++ pprint blah))))

main = putStrLn test
