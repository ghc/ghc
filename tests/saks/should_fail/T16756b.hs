{-# LANGUAGE StandaloneKindSignatures #-}

module T16756b where

import Data.Kind (Type)

type T :: Type -> Type
data T
  -- We must write:
  --   data T a
  -- See https://gitlab.haskell.org/ghc/ghc/issues/16756#note_203567
