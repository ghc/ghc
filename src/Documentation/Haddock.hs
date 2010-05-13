-----------------------------------------------------------------------------
-- |
-- Module      :  Documentation.Haddock
-- Copyright   :  (c) David Waern 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskellorg
-- Stability   :  experimental
-- Portability :  portable
--
-- The Haddock API: A rudimentory, highly experimental API exposing some of
-- the internals of Haddock. Don't expect it to be stable.
-----------------------------------------------------------------------------
module Documentation.Haddock (

  -- * Interface
  Interface(..),
  InstalledInterface(..),
  createInterfaces,

  -- * Export items & declarations
  ExportItem(..),
  Decl,
  DeclInfo,
  DocForDecl,
  FnArgsDoc, 
 
  -- * Hyperlinking
  LinkEnv,
  DocName(..),
  docNameOcc,

  -- * Instances
  DocInstance,
  InstHead,

  -- * Documentation comments
  Doc(..),
  Example(..),
  DocMarkup(..),
  HaddockModInfo(..),

  -- * Interface files
  -- | (.haddock files)
  InterfaceFile(..),
  readInterfaceFile,
  nameCacheFromGhc,
  freshNameCache,
  NameCacheAccessor,
  
  -- * Flags and options
  Flag(..),
  DocOption(..)

) where


import Haddock.InterfaceFile
import Haddock.Interface
import Haddock.Types
import Haddock.Options
