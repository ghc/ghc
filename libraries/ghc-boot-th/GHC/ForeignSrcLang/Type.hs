{-# LANGUAGE DeriveGeneric #-}
module GHC.ForeignSrcLang.Type
  ( ForeignSrcLang(..)
  ) where

import GHC.Generics (Generic)

data ForeignSrcLang
  = LangC | LangCxx | LangObjc | LangObjcxx
  deriving (Eq, Show, Generic)
