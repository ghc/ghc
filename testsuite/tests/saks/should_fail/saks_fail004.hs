{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds, DataKinds #-}

module SAKS_Fail004 where

import Data.Kind (Type)

-- See also: T16263
type Q :: Eq a => Type
data Q
