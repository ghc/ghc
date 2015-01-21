{-# LANGUAGE NoImplicitPrelude #-}
module Targets (
    targetPackages, targetPackagesInStage,
    IntegerLibrary (..), integerLibrary,
    buildHaddock
    ) where

import Package.Base

data IntegerLibrary = IntegerGmp | IntegerGmp2 | IntegerSimple

instance Show IntegerLibrary where
    show library = case library of
         IntegerGmp    -> "integer-gmp"
         IntegerGmp2   -> "integer-gmp2"
         IntegerSimple -> "integer-simple"

-- TODO: keep or move to configuration files? see Note [configuration files]
integerLibrary :: IntegerLibrary
integerLibrary = IntegerGmp2

integerLibraryName :: String
integerLibraryName = show integerLibrary

-- see Note [configuration files]
buildHaddock :: Bool
buildHaddock = True

-- These are the packages we build:
-- TODO: this should eventually be removed and replaced by the top-level
-- target, i.e. GHC (and perhaps, something else)
targetPackages :: [Package]
targetPackages =
    [   standardLibrary "array"            [        Stage1]
    ,     customLibrary "base"             [        Stage1] baseConfArgs
    ,   standardLibrary "bin-package-db"   [Stage0, Stage1]
    ,   standardLibrary "binary"           [Stage0, Stage1]
    ,   standardLibrary "bytestring"       [        Stage1]
    , customNameLibrary "Cabal/Cabal"      [Stage0, Stage1] cabalTraits
    ,   standardLibrary "containers"       [        Stage1]
    ,   standardLibrary "deepseq"          [        Stage1]
    ,   standardLibrary "directory"        [        Stage1]
    ,   standardLibrary "filepath"         [        Stage1]
    ,     customLibrary "ghc-prim"         [        Stage1] ghcPrimConfArgs
    ,   standardLibrary "haskeline"        [        Stage1]
    ,   standardLibrary "hoopl"            [Stage0, Stage1]
    ,   standardLibrary "hpc"              [Stage0, Stage1]
    , customNameLibrary integerLibraryName [        Stage1] integerLibTraits
    ,   standardLibrary "parallel"         [        Stage1]
    ,   standardLibrary "pretty"           [        Stage1]
    ,   standardLibrary "primitive"        [        Stage1]
    ,   standardLibrary "process"          [        Stage1]
    ,   standardLibrary "stm"              [        Stage1]
    ,   standardLibrary "template-haskell" [        Stage1]
    ,     customLibrary "terminfo"         [Stage0, Stage1] whenTerminfo
    ,   standardLibrary "time"             [        Stage1]
    ,   standardLibrary "transformers"     [Stage0, Stage1]
    ,     customLibrary "unix"             [        Stage1] whenUnix
    ,     customLibrary "Win32"            [        Stage1] whenWin32
    ,     customLibrary "xhtml"            [        Stage1] whenXhtml
    ]

baseConfArgs :: Settings -> Settings
baseConfArgs settings =
    settings { customConfArgs = arg $ "--flags=" ++ integerLibraryName }

-- see Note [Cabal package weirdness]
cabalTraits :: (String, Settings -> Settings)
cabalTraits = ("Cabal", id) -- change cabalName, keep other settings intact

ghcPrimConfArgs :: Settings -> Settings
ghcPrimConfArgs settings =
    settings { customConfArgs = arg "--flag=include-ghc-prim" }

integerLibTraits :: (String, Settings -> Settings)
integerLibTraits = (cabalName, traits)
  where
    cabalName = case integerLibrary of
        IntegerGmp    -> "integer-gmp"
        IntegerGmp2   -> "integer-gmp" -- Indeed, why make life easier?
        IntegerSimple -> "integer-simple"
    traits settings = settings
        {
            customConfArgs = when windowsHost $
                             arg "--configure-option=--with-intree-gmp",
            customCcArgs   = arg "-Ilibraries/integer-gmp2/gmp"
        }

whenTerminfo :: Settings -> Settings
whenTerminfo settings = settings
    {
        buildWhen = do
            os <- showArg TargetOs
            not windowsHost && (os /= "ios")
    }

whenUnix :: Settings -> Settings
whenUnix settings = settings { buildWhen = not windowsHost }

whenWin32 :: Settings -> Settings
whenWin32 settings = settings { buildWhen = windowsHost }

whenXhtml :: Settings -> Settings
whenXhtml settings = settings { buildWhen = return buildHaddock }

targetPackagesInStage :: Stage -> [Package]
targetPackagesInStage stage = filter inStage targetPackages
  where
    inStage (Package _ _ _ todoItems) = any matchStage todoItems
    matchStage (todoStage, _, _)    = todoStage == stage

-- TODISCUSS
-- Note [Cabal package weirdness]
-- Find out if we can move the contents to just Cabal/
-- What is Cabal/cabal-install? Do we need it?

-- TODISCUSS
-- Note [configuration files]
-- In this file we have two configuration options: integerLibrary and
-- buildHaddock. Arguably, their place should be among other configuration
-- options in the config files, however, moving integerLibrary there would
-- actually be quite painful, because it would then be confined to live in
-- the Action monad.
-- In general, shall we keep as many options as possible inside Shake, or
-- leave them in one place -- configuration files? We could try to move
-- everything to Shake which would be great:
--    * type safety and better abstractions
--    * useable outside the Action monad, e.g. for creating rules
--    * recompiling Shake is much faster then re-running configure script
--    * ... no more autoconf/configure and native Windows build?! Sign me up!
-- However, moving everything to Shake seems unfeasible at the moment.
