-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.TH.LanguageExtensions
-- Copyright   :  (c) The University of Glasgow 2015
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Language extensions known to GHC
--
-----------------------------------------------------------------------------

module Language.Haskell.TH.LanguageExtensions
   ( Extension(..)
   ) where

-- This module exists primarily to avoid inserting a massive list of language
-- extensions into the already quite large Haddocks for Language.Haskell.TH

import GHC.LanguageExtensions.Type (Extension(..))
