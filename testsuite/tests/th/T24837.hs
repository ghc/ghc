{-# LANGUAGE TemplateHaskell #-}
module T24837 where

import Language.Haskell.TH

$([d| f ((x :: Bool) :: Bool) = x |])
