{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Expression.Settings (
    Args (..), BuildParameter (..), EnvironmentParameter (..),
    Arity (..), Combine (..),
    Settings
    ) where

import Ways
import Base hiding (Args)
import Package
import Oracles.Builder
import Expression.Project
import Expression.Predicate
import Expression.BuildExpression

type Settings = BuildExpression Args

-- Settings comprise the following primitive elements
data Args
    = Plain String                              -- e.g. "-O2"
    | BuildParameter BuildParameter             -- e.g. build path
    | EnvironmentParameter EnvironmentParameter -- e.g. host OS
    | Fold Combine Settings                     -- e.g. ccSettings
    deriving (Show, Eq)

-- Build parameters to be determined during the build process
data BuildParameter
    = PackagePath -- path to the current package, e.g. "libraries/deepseq"
    | BuildDir    -- build directory, e.g. "dist-install"
    | Input       -- input file(s), e.g. "src.hs"
    | Output      -- output file(s), e.g. ["src.o", "src.hi"]
    deriving (Show, Eq)

-- Environment parameters to be determined using oracles
data EnvironmentParameter
    = BuilderPath Builder                -- look up path to a Builder
    | Config Arity String                -- look up configuration flag(s)
    | PackageData                        -- look up package-data.mk flag(s)
      {
        pdArity       :: Arity,          -- arity of value (Single or Multiple)
        pdKey         :: String,         -- key to look up, e.g. "PACKAGE_KEY"
        pdPackagePath :: Maybe FilePath, -- path to the current package
        pdBuildDir    :: Maybe FilePath  -- build directory
      }
    | PackageConstraints Packages        -- package version constraints
    deriving (Show, Eq)

-- Method for combining settings elements in Fold Combine Settings
data Combine = Id            -- Keep given settings as is
             | Concat        -- Concatenate: a ++ b
             | ConcatPath    -- </>-concatenate: a </> b
             | ConcatSpace   -- concatenate with a space: a ++ " " ++ b
             deriving (Show, Eq)

data Arity = Single   -- expands to a single argument
           | Multiple -- expands to a list of arguments
           deriving (Show, Eq)

-- Projecting on Way, Stage, Builder, FilePath and staged Builder is trivial:
-- only (Fold Combine Settings) and (EnvironmentParameter PackageConstraints)
-- can be affected (more specifically, the predicates contained in them).
-- This is handled with 'amap'.
amap :: (Project a Settings, Project a Packages) => a -> Args -> Args
amap p (Fold combine settings) = Fold combine (project p settings)
amap p (EnvironmentParameter (PackageConstraints ps)) =
    EnvironmentParameter $ PackageConstraints $ project p ps
amap _ a = a

instance Project Way Args where
    project = amap

instance Project Stage Args where
    project = amap

instance Project Builder Args where
    project = amap

instance Project FilePath Args where
    project = amap

instance Project (Stage -> Builder) Args where
    project = amap

-- Projecting on Package and TargetDir is more interesting.
instance Project Package Args where
    project p (BuildParameter PackagePath) = Plain $ pkgPath p
    project p (EnvironmentParameter pd @ (PackageData _ _ _ _)) =
        EnvironmentParameter $ pd { pdPackagePath = Just $ pkgPath p }
    project p a = amap p a

instance Project TargetDir Args where
    project (TargetDir d) (BuildParameter BuildDir) = Plain d
    project (TargetDir d) (EnvironmentParameter pd @ (PackageData _ _ _ _)) =
        EnvironmentParameter $ pd { pdBuildDir = Just d }
    project d a = amap d a
