{-# LANGUAGE TemplateHaskell #-}
module T5795 where

import Language.Haskell.TH

ty :: Q Type
ty = [t| Int |]

f :: $ty
f = undefined
