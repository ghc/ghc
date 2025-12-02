{-# LANGUAGE ExplicitLevelImports, TemplateHaskell, NoImplicitPrelude #-}
module T16568 where

x = $(do
  _ <- _
  _)

