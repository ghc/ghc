{-# LANGUAGE TemplateHaskell #-}
module T5971 where

import Language.Haskell.TH

_ = $(newName "x" >>= varE)
