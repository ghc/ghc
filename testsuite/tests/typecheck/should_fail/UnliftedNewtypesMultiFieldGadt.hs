{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- In tcConDecl, there is a place where a panic can happen if
-- a newtype has multiple fields. This test is here to make
-- sure that the appropriate validity checks happen before
-- we get to the panic. See Note [Kind-checking the field type].

module UnliftedNewtypesMultiFieldGadt where

import GHC.Exts
import Data.Kind

newtype Foo :: TYPE 'IntRep where
  FooC :: Bool -> Char -> Foo
