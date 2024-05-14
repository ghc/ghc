{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
module GHC.Internal.ForeignSrcLang
  ( ForeignSrcLang(..)
  ) where

#ifdef BOOTSTRAP_TH
import Prelude -- See note [Why do we import Prelude here?]
import GHC.Generics (Generic)
#else
import GHC.Internal.Base
import GHC.Internal.Show
import GHC.Internal.Generics
#endif

-- | Foreign formats supported by GHC via TH
data ForeignSrcLang
  = LangC      -- ^ C
  | LangCxx    -- ^ C++
  | LangObjc   -- ^ Objective C
  | LangObjcxx -- ^ Objective C++
  | LangAsm    -- ^ Assembly language (.s)
  | LangJs     -- ^ JavaScript
  | RawObject  -- ^ Object (.o)
  deriving (Eq, Show, Generic)
