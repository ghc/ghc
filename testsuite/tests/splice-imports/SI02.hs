{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitStageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI02 where

import splice Prelude
import Prelude

main :: IO ()
main = $(id [| pure () |])
