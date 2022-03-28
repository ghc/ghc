{-# LANGUAGE GADTs #-}

module GHC.Driver.Errors.Types (
    GhcMessage(..)
  , DriverMessage(..), DriverMessages, PsMessage(PsHeaderMessage)
  , BuildingCabalPackage(..)
  , WarningMessages
  , ErrorMessages
  , WarnMsg
  -- * Constructors
  , ghcUnknownMessage
  -- * Utility functions
  , hoistTcRnMessage
  , hoistDsMessage
  , checkBuildingCabalPackage
  ) where

import GHC.Prelude

import Data.Bifunctor
import Data.Typeable

import GHC.Driver.Session
import GHC.Types.Error
import GHC.Unit.Module
import GHC.Unit.State

import GHC.Parser.Errors.Types ( PsMessage(PsHeaderMessage) )
import GHC.Tc.Errors.Types     ( TcRnMessage )
import GHC.HsToCore.Errors.Types ( DsMessage )
import GHC.Hs.Extension          (GhcTc)

import Language.Haskell.Syntax.Decls (RuleDecl)

-- | A collection of warning messages.
-- /INVARIANT/: Each 'GhcMessage' in the collection should have 'SevWarning' severity.
type WarningMessages = Messages GhcMessage

-- | A collection of error messages.
-- /INVARIANT/: Each 'GhcMessage' in the collection should have 'SevError' severity.
type ErrorMessages   = Messages GhcMessage

-- | A single warning message.
-- /INVARIANT/: It must have 'SevWarning' severity.
type WarnMsg         = MsgEnvelope GhcMessage


{- Note [GhcMessage]
~~~~~~~~~~~~~~~~~~~~

We might need to report diagnostics (error and/or warnings) to the users. The
'GhcMessage' type is the root of the diagnostic hierarchy.

It's useful to have a separate type constructor for the different stages of
the compilation pipeline. This is not just helpful for tools, as it gives a
clear indication on where the error occurred exactly. Furthermore it increases
the modularity amongst the different components of GHC (i.e. to avoid having
"everything depend on everything else") and allows us to write separate
functions that renders the different kind of messages.

-}

-- | The umbrella type that encompasses all the different messages that GHC
-- might output during the different compilation stages. See
-- Note [GhcMessage].
data GhcMessage where
  -- | A message from the parsing phase.
  GhcPsMessage      :: PsMessage -> GhcMessage
  -- | A message from typecheck/renaming phase.
  GhcTcRnMessage    :: TcRnMessage -> GhcMessage
  -- | A message from the desugaring (HsToCore) phase.
  GhcDsMessage      :: DsMessage -> GhcMessage
  -- | A message from the driver.
  GhcDriverMessage  :: DriverMessage -> GhcMessage

  -- | An \"escape\" hatch which can be used when we don't know the source of
  -- the message or if the message is not one of the typed ones. The
  -- 'Diagnostic' and 'Typeable' constraints ensure that if we /know/, at
  -- pattern-matching time, the originating type, we can attempt a cast and
  -- access the fully-structured error. This would be the case for a GHC
  -- plugin that offers a domain-specific error type but that doesn't want to
  -- place the burden on IDEs/application code to \"know\" it. The
  -- 'Diagnostic' constraint ensures that worst case scenario we can still
  -- render this into something which can be eventually converted into a
  -- 'DecoratedSDoc'.
  GhcUnknownMessage :: forall a. (Diagnostic a, Typeable a) => a -> GhcMessage

-- | Creates a new 'GhcMessage' out of any diagnostic. This function is also
-- provided to ease the integration of #18516 by allowing diagnostics to be
-- wrapped into the general (but structured) 'GhcMessage' type, so that the
-- conversion can happen gradually. This function should not be needed within
-- GHC, as it would typically be used by plugin or library authors (see
-- comment for the 'GhcUnknownMessage' type constructor)
ghcUnknownMessage :: (Diagnostic a, Typeable a) => a -> GhcMessage
ghcUnknownMessage = GhcUnknownMessage

-- | Abstracts away the frequent pattern where we are calling 'ioMsgMaybe' on
-- the result of 'IO (Messages TcRnMessage, a)'.
hoistTcRnMessage :: Monad m => m (Messages TcRnMessage, a) -> m (Messages GhcMessage, a)
hoistTcRnMessage = fmap (first (fmap GhcTcRnMessage))

-- | Abstracts away the frequent pattern where we are calling 'ioMsgMaybe' on
-- the result of 'IO (Messages DsMessage, a)'.
hoistDsMessage :: Monad m => m (Messages DsMessage, a) -> m (Messages GhcMessage, a)
hoistDsMessage = fmap (first (fmap GhcDsMessage))

-- | A collection of driver messages
type DriverMessages = Messages DriverMessage

-- | A message from the driver.
data DriverMessage where
  -- | Simply wraps a generic 'Diagnostic' message @a@.
  DriverUnknownMessage :: (Diagnostic a, Typeable a) => a -> DriverMessage
  -- | A parse error in parsing a Haskell file header during dependency
  -- analysis
  DriverPsHeaderMessage :: !PsMessage -> DriverMessage

  {-| DriverMissingHomeModules is a warning (controlled with -Wmissing-home-modules) that
      arises when running GHC in --make mode when some modules needed for compilation
      are not included on the command line. For example, if A imports B, `ghc --make
      A.hs` will cause this warning, while `ghc --make A.hs B.hs` will not.

      Useful for cabal to ensure GHC won't pick up modules listed neither in
      'exposed-modules' nor in 'other-modules'.

      Test case: warnings/should_compile/MissingMod

  -}
  DriverMissingHomeModules :: [ModuleName] -> !BuildingCabalPackage -> DriverMessage

  {-| DriverUnknown is a warning that arises when a user tries to
      reexport a module which isn't part of that unit.
  -}
  DriverUnknownReexportedModules :: [ModuleName] -> DriverMessage

  {-| DriverUnknownHiddenModules is a warning that arises when a user tries to
      hide a module which isn't part of that unit.
  -}
  DriverUnknownHiddenModules :: [ModuleName] -> DriverMessage

  {-| DriverUnusedPackages occurs when when package is requested on command line,
      but was never needed during compilation. Activated by -Wunused-packages.

     Test cases: warnings/should_compile/UnusedPackages
  -}
  DriverUnusedPackages :: [(UnitId, PackageName, Version, PackageArg)] -> DriverMessage

  {-| DriverUnnecessarySourceImports (controlled with -Wunused-imports) occurs if there
      are {-# SOURCE #-} imports which are not necessary. See 'warnUnnecessarySourceImports'
      in 'GHC.Driver.Make'.

     Test cases: warnings/should_compile/T10637
  -}
  DriverUnnecessarySourceImports :: !ModuleName -> DriverMessage

  {-| DriverDuplicatedModuleDeclaration occurs if a module 'A' is declared in
       multiple files.

     Test cases: None.
  -}
  DriverDuplicatedModuleDeclaration :: !Module -> [FilePath] -> DriverMessage

  {-| DriverModuleNotFound occurs if a module 'A' can't be found.

     Test cases: None.
  -}
  DriverModuleNotFound :: !ModuleName -> DriverMessage

  {-| DriverFileModuleNameMismatch occurs if a module 'A' is defined in a file with a different name.
      The first field is the name written in the source code; the second argument is the name extracted
      from the filename.

     Test cases: module/mod178, /driver/bug1677
  -}
  DriverFileModuleNameMismatch :: !ModuleName -> !ModuleName -> DriverMessage

  {-| DriverUnexpectedSignature occurs when GHC encounters a module 'A' that imports a signature
      file which is neither in the 'signatures' section of a '.cabal' file nor in any package in
      the home modules.

      Example:

      -- MyStr.hsig is defined, but not added to 'signatures' in the '.cabal' file.
      signature MyStr where
          data Str

      -- A.hs, which tries to import the signature.
      module A where
      import MyStr


     Test cases: driver/T12955
  -}
  DriverUnexpectedSignature :: !ModuleName -> !BuildingCabalPackage -> GenInstantiations UnitId -> DriverMessage

  {-| DriverFileNotFound occurs when the input file (e.g. given on the command line) can't be found.

     Test cases: None.
  -}
  DriverFileNotFound :: !FilePath -> DriverMessage

  {-| DriverStaticPointersNotSupported occurs when the 'StaticPointers' extension is used
       in an interactive GHCi context.

     Test cases: ghci/scripts/StaticPtr
  -}
  DriverStaticPointersNotSupported :: DriverMessage

  {-| DriverBackpackModuleNotFound occurs when Backpack can't find a particular module
      during its dependency analysis.

     Test cases: -
  -}
  DriverBackpackModuleNotFound :: !ModuleName -> DriverMessage

  {-| DriverUserDefinedRuleIgnored is a warning that occurs when user-defined rules
      are ignored. This typically happens when Safe Haskell.

     Test cases:

       tests/safeHaskell/safeInfered/UnsafeWarn05
       tests/safeHaskell/safeInfered/UnsafeWarn06
       tests/safeHaskell/safeInfered/UnsafeWarn07
       tests/safeHaskell/safeInfered/UnsafeInfered11
       tests/safeHaskell/safeLanguage/SafeLang03
  -}
  DriverUserDefinedRuleIgnored :: !(RuleDecl GhcTc) -> DriverMessage

  {-| DriverMixedSafetyImport is an error that occurs when a module is imported
      both as safe and unsafe.

    Test cases:

      tests/safeHaskell/safeInfered/Mixed03
      tests/safeHaskell/safeInfered/Mixed02

  -}
  DriverMixedSafetyImport :: !ModuleName -> DriverMessage

  {-| DriverCannotLoadInterfaceFile is an error that occurs when we cannot load the interface
      file for a particular module. This can happen for example in the context of Safe Haskell,
      when we have to load a module to check if it can be safely imported.

    Test cases: None.

  -}
  DriverCannotLoadInterfaceFile :: !Module -> DriverMessage

  {-| DriverInferredSafeImport is a warning (controlled by the Opt_WarnSafe flag)
      that occurs when a module is inferred safe.

    Test cases: None.

  -}
  DriverInferredSafeModule :: !Module -> DriverMessage

  {-| DriverMarkedTrustworthyButInferredSafe is a warning (controlled by the Opt_WarnTrustworthySafe flag)
      that occurs when a module is marked trustworthy in SafeHaskell but it has been inferred safe.

    Test cases:
      tests/safeHaskell/safeInfered/TrustworthySafe02
      tests/safeHaskell/safeInfered/TrustworthySafe03

  -}
  DriverMarkedTrustworthyButInferredSafe :: !Module -> DriverMessage

  {-| DriverInferredSafeImport is a warning (controlled by the Opt_WarnInferredSafeImports flag)
      that occurs when a safe-inferred module is imported from a safe module.

    Test cases: None.

  -}
  DriverInferredSafeImport :: !Module -> DriverMessage

  {-| DriverCannotImportUnsafeModule is an error that occurs when an usafe module
      is being imported from a safe one.

    Test cases: None.

  -}
  DriverCannotImportUnsafeModule :: !Module -> DriverMessage

  {-| DriverMissingSafeHaskellMode is a warning (controlled by the Opt_WarnMissingSafeHaskellMode flag)
      that occurs when a module is using SafeHaskell features but SafeHaskell mode is not enabled.

    Test cases: None.

  -}
  DriverMissingSafeHaskellMode :: !Module -> DriverMessage

  {-| DriverPackageNotTrusted is an error that occurs when a package is required to be trusted
      but it isn't.

    Test cases:
      tests/safeHaskell/check/Check01
      tests/safeHaskell/check/Check08
      tests/safeHaskell/check/Check06
      tests/safeHaskell/check/pkg01/ImpSafeOnly09
      tests/safeHaskell/check/pkg01/ImpSafe03
      tests/safeHaskell/check/pkg01/ImpSafeOnly07
      tests/safeHaskell/check/pkg01/ImpSafeOnly08

  -}
  DriverPackageNotTrusted :: !UnitState -> !UnitId -> DriverMessage

  {-| DriverCannotImportFromUntrustedPackage is an error that occurs in the context of
      Safe Haskell when trying to import a module coming from an untrusted package.

    Test cases:
      tests/safeHaskell/check/Check09
      tests/safeHaskell/check/pkg01/ImpSafe01
      tests/safeHaskell/check/pkg01/ImpSafe04
      tests/safeHaskell/check/pkg01/ImpSafeOnly03
      tests/safeHaskell/check/pkg01/ImpSafeOnly05
      tests/safeHaskell/flags/SafeFlags17
      tests/safeHaskell/flags/SafeFlags22
      tests/safeHaskell/flags/SafeFlags23
      tests/safeHaskell/ghci/p11
      tests/safeHaskell/ghci/p12
      tests/safeHaskell/ghci/p17
      tests/safeHaskell/ghci/p3
      tests/safeHaskell/safeInfered/UnsafeInfered01
      tests/safeHaskell/safeInfered/UnsafeInfered02
      tests/safeHaskell/safeInfered/UnsafeInfered02
      tests/safeHaskell/safeInfered/UnsafeInfered03
      tests/safeHaskell/safeInfered/UnsafeInfered05
      tests/safeHaskell/safeInfered/UnsafeInfered06
      tests/safeHaskell/safeInfered/UnsafeInfered09
      tests/safeHaskell/safeInfered/UnsafeInfered10
      tests/safeHaskell/safeInfered/UnsafeInfered11
      tests/safeHaskell/safeInfered/UnsafeWarn01
      tests/safeHaskell/safeInfered/UnsafeWarn03
      tests/safeHaskell/safeInfered/UnsafeWarn04
      tests/safeHaskell/safeInfered/UnsafeWarn05
      tests/safeHaskell/unsafeLibs/BadImport01
      tests/safeHaskell/unsafeLibs/BadImport06
      tests/safeHaskell/unsafeLibs/BadImport07
      tests/safeHaskell/unsafeLibs/BadImport08
      tests/safeHaskell/unsafeLibs/BadImport09
      tests/safeHaskell/unsafeLibs/Dep05
      tests/safeHaskell/unsafeLibs/Dep06
      tests/safeHaskell/unsafeLibs/Dep07
      tests/safeHaskell/unsafeLibs/Dep08
      tests/safeHaskell/unsafeLibs/Dep09
      tests/safeHaskell/unsafeLibs/Dep10

  -}
  DriverCannotImportFromUntrustedPackage :: !UnitState -> !Module -> DriverMessage

  DriverRedirectedNoMain :: !ModuleName -> DriverMessage

  DriverHomePackagesNotClosed :: ![UnitId] -> DriverMessage

-- | Pass to a 'DriverMessage' the information whether or not the
-- '-fbuilding-cabal-package' flag is set.
data BuildingCabalPackage
  = YesBuildingCabalPackage
  | NoBuildingCabalPackage
  deriving Eq

-- | Checks if we are building a cabal package by consulting the 'DynFlags'.
checkBuildingCabalPackage :: DynFlags -> BuildingCabalPackage
checkBuildingCabalPackage dflags =
  if gopt Opt_BuildingCabalPackage dflags
     then YesBuildingCabalPackage
     else NoBuildingCabalPackage
