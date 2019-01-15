{-# LANGUAGE DeriveGeneric #-}
module GHC.ForeignSrcLang.Type
  ( ForeignSrcLang(..)
  ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Generics (Generic)

-- | Foreign formats supported by GHC via TH
data ForeignSrcLang
  = LangC      -- ^ C
  | LangCxx    -- ^ C++
  | LangObjc   -- ^ Objective C
  | LangObjcxx -- ^ Objective C++
  | LangAsm    -- ^ Assembly language (.s)
  | RawObject  -- ^ Object (.o)
  deriving (Eq, Show, Generic)
