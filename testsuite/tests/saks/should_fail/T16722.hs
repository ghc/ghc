{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE NoPolyKinds #-}

module T16722 where

import Data.Kind

type D :: k -> Type
data D a
