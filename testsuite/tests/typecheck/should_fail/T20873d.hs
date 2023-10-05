
{-# LANGUAGE GADTSyntax, NoKindSignatures, NoDataKinds #-}

module T20873d where

import Data.Kind ( Type )

type U a = Type

data Foo :: U Type where
  MkFoo :: Foo
