{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
module T12977 where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

zero :: () -> $(conT (unboxedTupleTypeName 0))
zero () =  $(conE (unboxedTupleDataName 0))

one :: () -> $(conT (unboxedTupleTypeName 1) `appT` conT ''Int)
one () =  $(conE (unboxedTupleDataName 1)) 42
