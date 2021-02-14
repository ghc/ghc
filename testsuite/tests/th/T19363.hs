{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}

module Main where

import Language.Haskell.TH

main =  runQ [d| data Operator = (:*) Int | (:**) { (^**) :: Int }

                 f = (:**) { (^**) = 42 }
                 infix 5 `f`

                 g (:**) { (^**) = x } = x

                 pattern a `H` b = a :** b
               |] >>= putStrLn . pprint
