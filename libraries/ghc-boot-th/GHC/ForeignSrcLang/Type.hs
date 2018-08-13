{-# LANGUAGE DeriveGeneric #-}
module GHC.ForeignSrcLang.Type
  ( ForeignSrcLang(..)
  ) where

import Prelude
import GHC.Generics (Generic)

data ForeignSrcLang
  = LangC | LangCxx | LangObjc | LangObjcxx | RawObject
  deriving (Eq, Show, Generic)
