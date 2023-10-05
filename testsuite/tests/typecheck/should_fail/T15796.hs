{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Bug where

newtype N a where
  MkN :: Show a => a -> N a
type family T a
type instance T (N a) = N a
