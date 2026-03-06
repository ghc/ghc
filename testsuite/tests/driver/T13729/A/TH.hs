{-# LANGUAGE TemplateHaskell #-}
module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Types

th_string = lift . show =<< reify ''Foo
