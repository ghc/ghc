{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module T10819 where

import T10819_Lib

import Language.Haskell.TH.Syntax

class C a b | b -> a where
  f :: b -> a

data D = X

instance C Int D where
  f X = 2

$(doSomeTH "N" (mkName "D")
    [DerivClause Nothing [ConT (mkName "C") `AppT` ConT (mkName "Int")]])

thing :: N
thing = N X

thing1 :: Int
thing1 = f thing
