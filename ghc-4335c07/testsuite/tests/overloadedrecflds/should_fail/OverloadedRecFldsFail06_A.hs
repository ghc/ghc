{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}

module OverloadedRecFldsFail06_A (U(..), V(..), Unused(unused), u, getX, getY, z) where

data U = MkU { x :: Bool, y :: Bool } | MkU2 { used_locally :: Bool }
  deriving Show
data V = MkV { x :: Int } | MkV2 { y :: Bool }
data Unused = MkUnused { unused :: Bool, unused2 :: Bool, used_locally :: Bool }

u = MkU False True

z MkU2{used_locally=used_locally} = used_locally

getX MkU{x=x} = x
getY MkV2{y=y} = y
