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
                [ derivClause (Just Stock)    [ [t| Eq   |] ]
                , derivClause (Just Anyclass) [ [t| C    |] ]
                , derivClause (Just Newtype)  [ [t| Read |] ] ]
             , standaloneDerivWithStrategyD (Just Stock)
                 (cxt []) [t| Ord $(fooType) |]
             , standaloneDerivWithStrategyD (Just Anyclass)
                 (cxt []) [t| D $(fooType) |]
             , standaloneDerivWithStrategyD (Just Newtype)
                 (cxt []) [t| Show $(fooType) |] ])
