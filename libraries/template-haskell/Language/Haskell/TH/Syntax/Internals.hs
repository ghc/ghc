{-# LANGUAGE GeneralizedNewtypeDeriving,DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Syntax.Internals
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Abstract syntax definitions for Template Haskell.
--
-----------------------------------------------------------------------------

module Language.Haskell.TH.Syntax.Internals (
    ModName(..), PkgName(..), OccName(..)
 ) where

import Data.Data

newtype ModName = ModName String	-- Module name
 deriving (Eq,Ord,Typeable,Data)

newtype PkgName = PkgName String	-- package name
 deriving (Eq,Ord,Typeable,Data)

newtype OccName = OccName String
 deriving (Eq,Ord,Typeable,Data)
