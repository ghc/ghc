{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module T7445a ( foo ) where

import Data.Data
import Language.Haskell.TH.Quote
import Language.Haskell.TH

data Expr
    =  IntExpr Integer
    deriving (Show, Typeable, Data)

foo :: ExpQ
foo = dataToExpQ (const Nothing) (IntExpr 1)
