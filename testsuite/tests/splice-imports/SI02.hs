{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI02 where

-- Splice importing a package module works
import splice Prelude
import Prelude

main :: IO ()
main = $(id [| pure () |])
