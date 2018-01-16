{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.SourceRepo (
    SourceRepo(..),
    RepoKind(..),
    RepoType(..),
    knownRepoTypes,
    emptySourceRepo,
    classifyRepoType,
    classifyRepoKind,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Utils.Generic (lowercase)

import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Text

import qualified Distribution.Compat.Parsec as P
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp

-- ------------------------------------------------------------
-- * Source repos
-- ------------------------------------------------------------

-- | Information about the source revision control system for a package.
--
-- When specifying a repo it is useful to know the meaning or intention of the
-- information as doing so enables automation. There are two obvious common
-- purposes: one is to find the repo for the latest development version, the
-- other is to find the repo for this specific release. The 'ReopKind'
-- specifies which one we mean (or another custom one).
--
-- A package can specify one or the other kind or both. Most will specify just
-- a head repo but some may want to specify a repo to reconstruct the sources
-- for this package release.
--
-- The required information is the 'RepoType' which tells us if it's using
-- 'Darcs', 'Git' for example. The 'repoLocation' and other details are
-- interpreted according to the repo type.
--
data SourceRepo = SourceRepo {
  -- | The kind of repo. This field is required.
  repoKind     :: RepoKind,

  -- | The type of the source repository system for this repo, eg 'Darcs' or
  -- 'Git'. This field is required.
  repoType     :: Maybe RepoType,

  -- | The location of the repository. For most 'RepoType's this is a URL.
  -- This field is required.
  repoLocation :: Maybe String,

  -- | 'CVS' can put multiple \"modules\" on one server and requires a
  -- module name in addition to the location to identify a particular repo.
  -- Logically this is part of the location but unfortunately has to be
  -- specified separately. This field is required for the 'CVS' 'RepoType' and
  -- should not be given otherwise.
  repoModule   :: Maybe String,

  -- | The name or identifier of the branch, if any. Many source control
  -- systems have the notion of multiple branches in a repo that exist in the
  -- same location. For example 'Git' and 'CVS' use this while systems like
  -- 'Darcs' use different locations for different branches. This field is
  -- optional but should be used if necessary to identify the sources,
  -- especially for the 'RepoThis' repo kind.
  repoBranch   :: Maybe String,

  -- | The tag identify a particular state of the repository. This should be
  -- given for the 'RepoThis' repo kind and not for 'RepoHead' kind.
  --
  repoTag      :: Maybe String,

  -- | Some repositories contain multiple projects in different subdirectories
  -- This field specifies the subdirectory where this packages sources can be
  -- found, eg the subdirectory containing the @.cabal@ file. It is interpreted
  -- relative to the root of the repository. This field is optional. If not
  -- given the default is \".\" ie no subdirectory.
  repoSubdir   :: Maybe FilePath
}
  deriving (Eq, Generic, Read, Show, Typeable, Data)

emptySourceRepo :: RepoKind -> SourceRepo
emptySourceRepo kind = SourceRepo
    { repoKind     = kind
    , repoType     = Nothing
    , repoLocation = Nothing
    , repoModule   = Nothing
    , repoBranch   = Nothing
    , repoTag      = Nothing
    , repoSubdir   = Nothing
    }

instance Binary SourceRepo

-- | What this repo info is for, what it represents.
--
data RepoKind =
    -- | The repository for the \"head\" or development version of the project.
    -- This repo is where we should track the latest development activity or
    -- the usual repo people should get to contribute patches.
    RepoHead

    -- | The repository containing the sources for this exact package version
    -- or release. For this kind of repo a tag should be given to give enough
    -- information to re-create the exact sources.
  | RepoThis

  | RepoKindUnknown String
  deriving (Eq, Generic, Ord, Read, Show, Typeable, Data)

instance Binary RepoKind

-- | An enumeration of common source control systems. The fields used in the
-- 'SourceRepo' depend on the type of repo. The tools and methods used to
-- obtain and track the repo depend on the repo type.
--
data RepoType = Darcs | Git | SVN | CVS
              | Mercurial | GnuArch | Bazaar | Monotone
              | OtherRepoType String
  deriving (Eq, Generic, Ord, Read, Show, Typeable, Data)

instance Binary RepoType

knownRepoTypes :: [RepoType]
knownRepoTypes = [Darcs, Git, SVN, CVS
                 ,Mercurial, GnuArch, Bazaar, Monotone]

repoTypeAliases :: RepoType -> [String]
repoTypeAliases Bazaar    = ["bzr"]
repoTypeAliases Mercurial = ["hg"]
repoTypeAliases GnuArch   = ["arch"]
repoTypeAliases _         = []

instance Pretty RepoKind where
  pretty RepoHead                = Disp.text "head"
  pretty RepoThis                = Disp.text "this"
  pretty (RepoKindUnknown other) = Disp.text other

instance Parsec RepoKind where
  parsec = classifyRepoKind <$> P.munch1 isIdent

instance Text RepoKind where
  parse = fmap classifyRepoKind ident

classifyRepoKind :: String -> RepoKind
classifyRepoKind name = case lowercase name of
  "head" -> RepoHead
  "this" -> RepoThis
  _      -> RepoKindUnknown name

instance Pretty RepoType where
  pretty (OtherRepoType other) = Disp.text other
  pretty other                 = Disp.text (lowercase (show other))

instance Parsec RepoType where
  parsec = classifyRepoType <$> P.munch1 isIdent

instance Text RepoType where
  parse = fmap classifyRepoType ident

classifyRepoType :: String -> RepoType
classifyRepoType s =
    fromMaybe (OtherRepoType s) $ lookup (lowercase s) repoTypeMap
  where
    repoTypeMap = [ (name, repoType')
                  | repoType' <- knownRepoTypes
                  , name <- display repoType' : repoTypeAliases repoType' ]

ident :: Parse.ReadP r String
ident = Parse.munch1 isIdent

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_' || c == '-'
