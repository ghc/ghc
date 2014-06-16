{-# LANGUAGE TemplateHaskell #-}
module T2685a (th) where

import Language.Haskell.TH

newtype NT = C (() -> ())

th :: Q [Dec]
th = [d| foo = C undefined |]
