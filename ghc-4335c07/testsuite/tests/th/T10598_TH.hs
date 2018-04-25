{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module T10598_TH where

import Language.Haskell.TH

class C a
instance C Int

class C a => D a
instance D Int

{-
newtype Foo = MkFoo Int
  deriving stock    Eq
  deriving anyclass C
  deriving newtype  Read

deriving stock    instance Ord  Foo
deriving anyclass instance D    Foo
deriving newtype  instance Show Foo
-}

$(do fooDataName  <- newName "Foo"
     mkFooConName <- newName "MkFoo"
     let fooType = conT fooDataName
     sequence [ newtypeD (cxt []) fooDataName [] Nothing
                (normalC mkFooConName
                  [ bangType (bang noSourceUnpackedness noSourceStrictness)
                             [t| Int |] ])
                [ derivClause (Just StockStrategy)    [ [t| Eq   |] ]
                , derivClause (Just AnyclassStrategy) [ [t| C    |] ]
                , derivClause (Just NewtypeStrategy)  [ [t| Read |] ] ]
             , standaloneDerivWithStrategyD (Just StockStrategy)
                 (cxt []) [t| Ord $(fooType) |]
             , standaloneDerivWithStrategyD (Just AnyclassStrategy)
                 (cxt []) [t| D $(fooType) |]
             , standaloneDerivWithStrategyD (Just NewtypeStrategy)
                 (cxt []) [t| Show $(fooType) |] ])
