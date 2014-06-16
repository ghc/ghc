{-# LANGUAGE TemplateHaskell #-}
module T8577 where

import Language.Haskell.TH

import T8577a

foo2 :: A Bool
foo2 = $$(y)

