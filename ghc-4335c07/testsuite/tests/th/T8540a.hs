{-# LANGUAGE TemplateHaskell #-}
module T8540a (foo) where

import Language.Haskell.TH

foo :: Q Exp
foo = [| bar |]

bar :: Int
bar = 5
