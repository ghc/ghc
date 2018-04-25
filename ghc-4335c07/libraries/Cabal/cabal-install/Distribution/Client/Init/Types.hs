{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Init.Types
-- Copyright   :  (c) Brent Yorgey, Benedikt Huber 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Some types used by the 'cabal init' command.
--
-----------------------------------------------------------------------------
module Distribution.Client.Init.Types where

import Distribution.Simple.Setup
  ( Flag(..) )

import Distribution.Types.Dependency as P
import Distribution.Compat.Semigroup
import Distribution.Version
import Distribution.Verbosity
import qualified Distribution.Package as P
import Distribution.License
import Distribution.ModuleName
import Language.Haskell.Extension ( Language(..), Extension )

import qualified Text.PrettyPrint as Disp
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Text

import GHC.Generics ( Generic )

-- | InitFlags is really just a simple type to represent certain
--   portions of a .cabal file.  Rather than have a flag for EVERY
--   possible field, we just have one for each field that the user is
--   likely to want and/or that we are likely to be able to
--   intelligently guess.
data InitFlags =
    InitFlags { nonInteractive :: Flag Bool
              , quiet          :: Flag Bool
              , packageDir     :: Flag FilePath
              , noComments     :: Flag Bool
              , minimal        :: Flag Bool

              , packageName  :: Flag P.PackageName
              , version      :: Flag Version
              , cabalVersion :: Flag VersionRange
              , license      :: Flag License
              , author       :: Flag String
              , email        :: Flag String
              , homepage     :: Flag String

              , synopsis     :: Flag String
              , category     :: Flag (Either String Category)
              , extraSrc     :: Maybe [String]

              , packageType  :: Flag PackageType
              , mainIs       :: Flag FilePath
              , language     :: Flag Language

              , exposedModules :: Maybe [ModuleName]
              , otherModules   :: Maybe [ModuleName]
              , otherExts      :: Maybe [Extension]

              , dependencies :: Maybe [P.Dependency]
              , sourceDirs   :: Maybe [String]
              , buildTools   :: Maybe [String]

              , initVerbosity :: Flag Verbosity
              , overwrite     :: Flag Bool
              }
  deriving (Show, Generic)

  -- the Monoid instance for Flag has later values override earlier
  -- ones, which is why we want Maybe [foo] for collecting foo values,
  -- not Flag [foo].

data PackageType = Library | Executable
  deriving (Show, Read, Eq)

instance Text PackageType where
  disp = Disp.text . show
  parse = Parse.choice $ map (fmap read . Parse.string . show) [Library, Executable] -- TODO: eradicateNoParse

instance Monoid InitFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup InitFlags where
  (<>) = gmappend

-- | Some common package categories.
data Category
    = Codec
    | Concurrency
    | Control
    | Data
    | Database
    | Development
    | Distribution
    | Game
    | Graphics
    | Language
    | Math
    | Network
    | Sound
    | System
    | Testing
    | Text
    | Web
    deriving (Read, Show, Eq, Ord, Bounded, Enum)

instance Text Category where
  disp  = Disp.text . show
  parse = Parse.choice $ map (fmap read . Parse.string . show) [Codec .. ] -- TODO: eradicateNoParse

