{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE CPP,
             DeriveGeneric,
             DeriveDataTypeable,
             LambdaCase,
             MagicHash,
             StandaloneDeriving
  #-}
#ifndef __GHCJS__
{-# LANGUAGE PackageImports #-}
#endif

{- |
     Communication between the compiler (GHCJS) and runtime (on node.js) for
     Template Haskell
 -}

module GHCJS.Prim.TH.Types ( Message(..)
                           , THResultType(..)
                           ) where

import           Prelude

import           Data.Binary
import           Data.ByteString (ByteString)
import           Data.Data

import           GHC.Generics

import           GHCi.TH.Binary ()

#if defined(__GHCJS__) || !defined(MIN_VERSION_template_haskell_ghcjs)
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH
#else
import qualified "template-haskell-ghcjs" Language.Haskell.TH        as TH
import qualified "template-haskell-ghcjs" Language.Haskell.TH.Syntax as TH
#endif

data THResultType = THExp | THPat | THType | THDec | THAnnWrapper
  deriving (Enum, Show, Data, Generic)

data Message
  -- | compiler to node requests
  = RunTH THResultType ByteString (Maybe TH.Loc)
  | FinishTH          Bool       -- ^ also stop runner (False to just clean up at end of module)
  -- | node to compiler responses
  | RunTH'            ByteString -- ^ serialized result
  | FinishTH'         Int        -- ^ memory consumption
  -- | node to compiler requests
  | NewName             String
  | Report              Bool String
  | LookupName          Bool String
  | Reify               TH.Name
  | ReifyInstances      TH.Name [TH.Type]
  | ReifyRoles          TH.Name
  | ReifyAnnotations    TH.AnnLookup
  | ReifyModule         TH.Module
  | ReifyFixity         TH.Name
  | ReifyConStrictness  TH.Name
  | AddForeignFilePath  TH.ForeignSrcLang String
  | AddDependentFile    FilePath
  | AddTempFile         String
  | AddTopDecls         [TH.Dec]
  | AddCorePlugin       String
  | IsExtEnabled        TH.Extension
  | ExtsEnabled
  -- | compiler to node responses
  | NewName'            TH.Name
  | Report'
  | LookupName'         (Maybe TH.Name)
  | Reify'              TH.Info
  | ReifyInstances'     [TH.Dec]
  | ReifyRoles'         [TH.Role]
  | ReifyAnnotations'   [ByteString]
  | ReifyModule'        TH.ModuleInfo
  | ReifyFixity'        (Maybe TH.Fixity)
  | ReifyConStrictness' [TH.DecidedStrictness]
  | AddForeignFilePath'
  | AddDependentFile'
  | AddTempFile'        FilePath
  | AddTopDecls'
  | AddCorePlugin'
  | IsExtEnabled'       Bool
  | ExtsEnabled'        [TH.Extension]
  | QFail'
  | QCompilerException' Int String -- ^ exception id and result of showing the exception
  -- | error recovery
  | StartRecover
  | EndRecover Bool            -- ^ true for recovery action taken
  | StartRecover'
  | EndRecover'
  -- | exit with error status
  | QFail              String  -- ^ monadic fail called
  | QUserException     String  -- ^ exception in user code
  | QCompilerException Int     -- ^ exception originated on compiler side
  deriving (Generic)


-- deriving instance Generic TH.ForeignSrcLang

instance Binary TH.Extension
instance Binary TH.ForeignSrcLang

instance Binary THResultType
instance Binary Message
