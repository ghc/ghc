{-# LANGUAGE TemplateHaskell #-}
module T15985 where

import Language.Haskell.TH

foo :: String
foo = $(stringE (pprint EqualityT))
