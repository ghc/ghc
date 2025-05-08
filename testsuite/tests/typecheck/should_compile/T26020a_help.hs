{-# LANGUAGE TypeFamilies #-}

module T26020a_help where

type family F a
newtype N a = MkN (F a)

