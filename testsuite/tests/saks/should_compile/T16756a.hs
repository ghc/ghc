{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module T16756a where

import Data.Kind (Type)

type T :: Type -> Type
data family T
  -- We do /not/ need to write:
  --   data family T a
  -- See https://gitlab.haskell.org/ghc/ghc/issues/16756#note_203567
