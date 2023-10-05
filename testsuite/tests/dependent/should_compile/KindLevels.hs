{-# LANGUAGE DataKinds, PolyKinds #-}

module KindLevels where

import Data.Kind

data A
data B :: A -> Type
data C :: B a -> Type
data D :: C b -> Type
data E :: D c -> Type
