{-# OPTIONS_GHC -fwarn-unused-binds #-}

module T10347 (N, mkN) where

import Data.Coerce

newtype N a = MkN Int

mkN :: Int -> N a
mkN = coerce
