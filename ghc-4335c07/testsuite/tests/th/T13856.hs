{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module T13856 where

import Language.Haskell.TH

f :: Int
f = $(lamE [] [| 42 |])
