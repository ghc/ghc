{-# LANGUAGE OverloadedRecordFields #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}

module OverloadedRecFldsFail06_A (U(..), V(..), Unused(unused), u, getX, getY, z) where

data U = MkU { x :: Bool, y :: Bool } | MkU2 { used_locally :: Bool }
  deriving Show
data V = MkV { x :: Int } | MkV2 { y :: Bool }
data Unused = MkUnused { unused :: Bool, unused2 :: Bool, used_locally :: Bool }

u = MkU False True

z r = used_locally r

getX r = x r
getY r = y r
