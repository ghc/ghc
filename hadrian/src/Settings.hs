module Settings (
    getArgs, getLibraryWays, getRtsWays, flavour, knownPackages,
    findPackageByName, unsafeFindPackageByName, unsafeFindPackageByPath,
    isLibrary, stagePackages, getIntegerPackage, completeSetting
    ) where

import CommandLine
import Expression
import Flavour
import Packages
import Settings.Parser
import UserSettings (userFlavours, userPackages, userDefaultFlavour)

import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Benchmark
import Settings.Flavours.Development
import Settings.Flavours.Llvm
import Settings.Flavours.Performance
import Settings.Flavours.Profiled
import Settings.Flavours.Quick
import Settings.Flavours.Quickest
import Settings.Flavours.QuickCross
import Settings.Flavours.GhcInGhci
import Settings.Flavours.Validate

import Control.Monad.Except
import Data.Either

getArgs :: Args
getArgs = expr flavour >>= args

getLibraryWays :: Ways
getLibraryWays = expr flavour >>= libraryWays

getRtsWays :: Ways
getRtsWays = expr flavour >>= rtsWays

stagePackages :: Stage -> Action [Package]
stagePackages stage = do
    f <- flavour
    packages f stage

hadrianFlavours :: [Flavour]
hadrianFlavours =
    [ benchmarkFlavour, defaultFlavour, developmentFlavour Stage1
    , developmentFlavour Stage2, performanceFlavour, profiledFlavour
    , quickFlavour, quickestFlavour, quickCrossFlavour, benchmarkLlvmFlavour
    , performanceLlvmFlavour, profiledLlvmFlavour, quickLlvmFlavour
    , ghcInGhciFlavour, validateFlavour, slowValidateFlavour ]

-- | This action looks up a flavour with the name given on the
--   command line with @--flavour@, defaulting to 'userDefaultFlavour'
--   when no explicit @--flavour@ is passed. It then applies any
--   potential setting update specified on the command line or in a
--   <build root>/hadrian.settings file, using @k = v@ or @k += v@ style
--   syntax. See Note [Hadrian settings] at the bottom of this file.
flavour :: Action Flavour
flavour = do
    flavourName <- fromMaybe userDefaultFlavour <$> cmdFlavour
    kvs <- userSetting ([] :: [KeyVal])
    let flavours = hadrianFlavours ++ userFlavours
        (_settingErrs, tweak) = applySettings kvs

    return $
      case filter (\fl -> name fl == flavourName) flavours of
        []  -> error $ "Unknown build flavour: " ++ flavourName
        [f] -> tweak f
        _   -> error $ "Multiple build flavours named " ++ flavourName

getIntegerPackage :: Expr Package
getIntegerPackage = expr (integerLibrary =<< flavour)

-- TODO: switch to Set Package as the order of packages should not matter?
-- Otherwise we have to keep remembering to sort packages from time to time.
knownPackages :: [Package]
knownPackages = sort $ ghcPackages ++ userPackages

-- TODO: Speed up? Switch to Set?
-- Note: this is slow but we keep it simple as there are just ~50 packages
findPackageByName :: PackageName -> Maybe Package
findPackageByName name = find (\pkg -> pkgName pkg == name) knownPackages

unsafeFindPackageByName :: PackageName -> Package
unsafeFindPackageByName name = fromMaybe (error msg) $ findPackageByName name
  where
    msg = "unsafeFindPackageByName: No package with name " ++ name

unsafeFindPackageByPath :: FilePath -> Package
unsafeFindPackageByPath path = err $ find (\pkg -> pkgPath pkg == path) knownPackages
  where
    err = fromMaybe $ error ("findPackageByPath: No package for path " ++ path)

-- * CLI and <root>/hadrian.settings options

{-
Note [Hadrian settings]
~~~~~~~~~~~~~~~~~~~~~~~

Hadrian lets one customize GHC builds through the UserSettings module,
where Hadrian users can override existing 'Flavour's or create entirely
new ones, overriding/extending the options passed to some builder
building the RTS in more ways and much more.

It now also offers a more "old-school" interface, in the form of
@foo.bar.baz = v@ or @foo.bar.baz += v@ expressions, that one can
pass on the command line that invokes hadrian:

> $ hadrian/build --flavour=quickest -j "stage1.ghc-bin.ghc.link.opts += -v3"

or in a file at <build root>/hadrian.settings, where <build root>
is the build root to be used for the build, which is _build by default.
For example, you could create a file at _build/hadrian.settings with the
following contents:

> stage1.ghc-bin.ghc.link.opts += -v3
> stage1.base.ghc.hs.opts += -ddump-timings

and issue:

> $ hadrian/build

Hadrian would pick up the settings given in _build/hadrian.settings (as well as
any settings that you may additionally be passing on the command line) and
update the relevant flavour accordingly, to issue the additional arguments
specified by the user.

The supported settings are described by 'builderSetting' below, using
operations from Applicative + two new primitives, 'matchString' and
'matchOneOf', that come as members of the 'Match' class. This gives us
a simple but powerful vocabulary to describe settings and parse them
into values that we can use to compute interesting things, like a 'Predicate'
that we can use to emit additional arguments, or a list of possible completions.

> fmap, (<$>) :: Functor f => (a -> b) -> f a -> f b
> pure :: Applicative f => a -> f a
> (<*>) :: Applicative f => f (a -> b) -> f a -> f b
> (*>) :: Applicative f => f a -> f b -> f b
> (<*) :: Applicative f => f a -> f b -> f a
> (<$) :: Functor f => a -> f b -> f a
>
> str :: Match f => String -> f ()
> val :: Match f => String -> a -> f a
> oneOf :: Match f => [f a] -> f a
> choose :: Match f => [(String, a)] -> f a
> wild :: Match f => [(String, a)] -> f (Wildcard a)

For instance, to describe possible settings:
  foo.bar.{x, y}
  foo.baz.{a, b}.c

we could write:

> str "foo" *> oneOf [ str "bar" *> choose [ ("x", "x"), ("y", "y") ]
>                    , str "baz" *> choose [ ("a", "ac"), ("b", "bc") <* str "c" ]
>                    ]

'builderSetting' uses these combinators to describe the setting keys that
Hadrian supports. A user-oriented description of this mechanism is available
in hadrian/doc/user-settings.md.

-}

-- | Try to interpret all the 'KeyVal' as flavour updates, keeping
--   a list of errors for the ones which don't match known
--   settings.
applySettings :: [KeyVal] -> ([SettingError], Flavour -> Flavour)
applySettings kvs = case partitionEithers (map applySetting kvs) of
  (errs, fs) -> (errs, foldr (flip (.)) id fs)
  -- we need to compose the reverse way to have the following settings
  --     x  = -b
  --     x += -c
  -- produce the final x = "-b -c" value. With just (.) we would apply
  -- the x = -b assignment last, which would silently drop the -c adddition.
  --
  --     foldr (.) id [f, g, h] = f . g . h
  --        -- first function (f) is applied last, we're applying them in
  --        -- the wrong order!
  --
  --     foldr (flip (.)) id [f, g, h] = h . g . f
  --        -- last function (f) is applied last, as desired


-- | Try to interpret the given 'KeyVal' as a flavour update
--   function, returning an error if it doesn't match a known
--   setting.
applySetting :: KeyVal -> Either SettingError (Flavour -> Flavour)
applySetting (KeyVal ks op v) = case runSettingsM ks builderPredicate of
  Left err -> throwError $
      "error while setting " ++ show ks ++ ": " ++ err
  Right pred -> Right $ \flav -> flav
    { args = update (args flav) pred }

  where override arguments predicate = do
          holds <- predicate
          if holds then pure (words v) else arguments

        augment arguments predicate =
          mconcat [arguments, predicate ? pure (words v)]

        update
          | op == Equal = override
          | otherwise   = augment

-- | Try to auto-complete the given @Key@ using
--   all known settings, as described by 'builderSetting'.
--
-- > completeSetting ["stage1","base", "ghc"]
-- >   -- returns [ ["stage1","base","ghc","c","opts"]
-- >   --         , ["stage1","base","ghc","hs","opts"]
-- >   --         , ["stage1","base","ghc","link","opts"]
-- >   --         , ["stage1","base","ghc","deps","opts"]
-- >   --         , ["stage1","base","ghc","toolargs","opts"]
-- >   --         ]
completeSetting :: Key -> [Key]
completeSetting ks = map snd (complete ks builderSetting)

-- | Interpret a 'builderSetting' as a 'Predicate' that
--   potentially constrains on the stage, package or
--   (ghc or cc) builder mode.
--
--   For example, @stage1.base.ghc.link.opts@ gets mapped to
--   a predicate that applies @'stage' 'Stage1'@,
--   @'package' 'base'@ and @'builder' ('Ghc' 'LinkHs')@.
builderPredicate :: SettingsM Predicate
builderPredicate = builderSetting <&> (\(wstg, wpkg, builderMode) ->
  wildcard (pure True) stage wstg <&&>
  wildcard (pure True) package wpkg <&&>
  (case builderMode of
     BM_Ghc ghcMode -> wildcard (builder Ghc) (builder . Ghc) ghcMode
     BM_Cc  ccMode  -> wildcard (builder Cc) (builder . Cc) ccMode
     BM_CabalConfigure -> builder (Cabal Setup) )
  )

  where (<&&>) = liftA2 (&&)

-- | Which builder a setting should apply to
data BuilderMode = BM_Ghc (Wildcard GhcMode)
                 | BM_Cc  (Wildcard CcMode)
                 | BM_CabalConfigure

-- | Interpretation-agnostic description of the builder settings
--   supported by Hadrian.
--
--   Supported settings (to be kept in sync with the code):
--
--   > (<stage> or *).(<package name> or *).ghc.(<ghc mode> or *).opts
--   > (<stage> or *).(<package name> or *).cc.(<cc mode> or *).opts
--   > (<stage> or *).(<package name> or *).cabal.configure.opts
--
--   where:
--     - @<stage>@ is one of @stage0@, @stage1@, @stage2@ or @stage3@;
--     - @<package name>@ is the (Cabal) name of a package (@base@,
--       @template-haskell@, ...);
--     - @<ghc mode>@ is one of @c@ (building C files), @hs@ (building Haskell
--       modules), @link@ (linking object files), @deps@ (finding Haskell
--       dependencies with @ghc -M@) or @toolargs@ (getting necessary flags to
--       make hadrian/ghci work;
--     - @<cc mode>@ is one of @c@ (building C files) or @deps@ (finding C
--       dependencies);
--     - locations that support a wildcard (@*@) entry are here to avoid
--       repetition, a wildcard entry being equivalent to writing all the
--       settings that the wildcard matches over; in our case, we can
--       apply GHC or C compiler options uniformly over all stages, packages
--       and compiler modes, if we so desire, by using a wildcard in the
--       appropriate spot.
builderSetting :: Match f
               => f (Wildcard Stage, Wildcard Package, BuilderMode)
builderSetting = (,,)
             <$> wild stages
             <*> wild pkgs
             <*> matchOneOf
                   [ str "ghc" *> fmap BM_Ghc (wild ghcBuilder) <* str "opts"
                   , str "cc" *> fmap BM_Cc (wild ccBuilder) <* str "opts"
                   , BM_CabalConfigure <$ str "cabal" <* str "configure" <* str "opts"
                   ]

  where ghcBuilder =
          [ ("c", CompileCWithGhc)
          , ("deps", FindHsDependencies)
          , ("hs", CompileHs)
          , ("link", LinkHs)
          , ("toolargs", ToolArgs)
          ]

        ccBuilder =
          [ ("c", CompileC)
          , ("deps", FindCDependencies)
          ]

        stages = map (\stg -> (stageString stg, stg)) [minBound..maxBound]

        pkgs = map (\pkg -> (pkgName pkg, pkg)) knownPackages
