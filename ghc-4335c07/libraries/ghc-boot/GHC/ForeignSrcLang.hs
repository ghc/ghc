{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | See @GHC.LanguageExtensions@ for an explanation
-- on why this is needed
module GHC.ForeignSrcLang
  ( module GHC.ForeignSrcLang.Type
  ) where

import Data.Binary
import GHC.ForeignSrcLang.Type

instance Binary ForeignSrcLang
