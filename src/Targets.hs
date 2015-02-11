{-# LANGUAGE NoImplicitPrelude #-}
module Targets (
    targetPackages, targetPackagesInStage
    ) where

import Package.Base
import Settings

-- These are the packages we build:
-- TODO: this should eventually be removed and replaced by the top-level
-- target, i.e. GHC (and perhaps, something else)
targetPackages :: [Package]
targetPackages =
    [ library "array"            [        Stage1]
    , library "base"             [        Stage1] `customise` baseTraits
    , library "bin-package-db"   [Stage0, Stage1]
    , library "binary"           [Stage0, Stage1]
    , library "bytestring"       [        Stage1]
    , library "Cabal/Cabal"      [Stage0, Stage1] `customise` cabalTraits
    , library "containers"       [        Stage1]
    , library "deepseq"          [        Stage1]
    , library "directory"        [        Stage1]
    , library "filepath"         [        Stage1]
    , library "ghc-prim"         [        Stage1] `customise` ghcPrimTraits
    , library "haskeline"        [        Stage1]
    , library "hoopl"            [Stage0, Stage1]
    , library "hpc"              [Stage0, Stage1]
    , library integerLibraryName [        Stage1] `customise` intLibTraits
    , library "parallel"         [        Stage1]
    , library "pretty"           [        Stage1]
    , library "primitive"        [        Stage1]
    , library "process"          [        Stage1]
    , library "stm"              [        Stage1]
    , library "template-haskell" [        Stage1]
    , library "terminfo"         [Stage0, Stage1] `customise` terminfoTraits
    , library "time"             [        Stage1]
    , library "transformers"     [Stage0, Stage1]
    , library "unix"             [        Stage1] `customise` unixTraits
    , library "Win32"            [        Stage1] `customise` win32Traits
    , library "xhtml"            [        Stage1] `customise` xhtmlTraits
    ]

baseTraits :: Package -> Package
baseTraits = updateSettings (\settings ->
    settings { customConfArgs = arg $ "--flags=" ++ integerLibraryName })

-- see Note [Cabal package weirdness]
cabalTraits :: Package -> Package
cabalTraits (Package name path cabal todo) = Package name path "Cabal" todo

ghcPrimTraits :: Package -> Package
ghcPrimTraits = updateSettings (\settings ->
    settings { customConfArgs = arg "--flag=include-ghc-prim" })

intLibTraits :: Package -> Package
intLibTraits (Package name path cabal todo) = updateSettings update pkg
  where
    pkg = Package name path cabalName todo
    cabalName = case integerLibrary of
        IntegerGmp    -> "integer-gmp"
        IntegerGmp2   -> "integer-gmp" -- Indeed, why make life easier?
        IntegerSimple -> "integer-simple"
    update settings = settings
        {
            customConfArgs = when windowsHost $
                             arg "--configure-option=--with-intree-gmp",
            customCcArgs   = arg "-Ilibraries/integer-gmp2/gmp"
        }

terminfoTraits :: Package -> Package
terminfoTraits = updateSettings (\settings ->
    settings
    {
        buildWhen = do
            os <- showArg TargetOs
            not windowsHost && (os /= "ios")
    })

unixTraits :: Package -> Package
unixTraits = updateSettings (\settings ->
    settings { buildWhen = not windowsHost })

win32Traits :: Package -> Package
win32Traits = updateSettings (\settings ->
    settings { buildWhen = windowsHost })

xhtmlTraits :: Package -> Package
xhtmlTraits = updateSettings (\settings ->
    settings { buildWhen = return buildHaddock })

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
