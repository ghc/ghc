{-# LANGUAGE TemplateHaskell #-}
module T23944A where

import Language.Haskell.TH

foo :: DecsQ
foo = pure []
