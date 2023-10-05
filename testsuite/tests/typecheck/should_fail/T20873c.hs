
{-# LANGUAGE GADTSyntax, NoKindSignatures, NoDataKinds #-}

module T20873c where

import Data.Kind ( Type )

type U a = Type

data Foo :: U Int where
  MkFoo :: Foo
