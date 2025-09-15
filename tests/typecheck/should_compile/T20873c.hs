
{-# LANGUAGE GADTSyntax, KindSignatures, NoDataKinds #-}

module T20873c where

import Data.Kind ( Type )

type U a = Type

-- This should be allowed without enabling DataKinds, This is because the return
-- kind only mentions Type, which is always permitted in kinds, and U, which is
-- simply a type synonym that expands to Type.
data Foo :: U Type where
  MkFoo :: Foo
