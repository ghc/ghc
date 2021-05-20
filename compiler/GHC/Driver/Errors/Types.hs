{-# LANGUAGE GADTs #-}

module GHC.Driver.Errors.Types (
    GhcMessage(..)
  , DriverMessage(..), DriverMessages
  , BuildingCabalPackage(..)
  , WarningMessages
  , ErrorMessages
  , WarnMsg
  -- * Constructors
  , ghcUnknownMessage
  -- * Utility functions
  , hoistTcRnMessage
  , hoistDsMessage
  , foldPsMessages
  , checkBuildingCabalPackage
  ) where

import GHC.Prelude

import Data.Bifunctor
import Data.Typeable

import GHC.Driver.Session
import GHC.Types.Error
import GHC.Unit.Module

import GHC.Parser.Errors       ( PsErrorDesc )
import GHC.Parser.Errors.Types ( PsMessage )
import GHC.Tc.Errors.Types     ( TcRnMessage )
import GHC.HsToCore.Errors.Types ( DsMessage )

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

-- | Given a collection of @e@ wrapped in a 'Foldable' structure, converts it
-- into 'Messages' via the supplied transformation function.
foldPsMessages :: Foldable f
               => (e -> MsgEnvelope PsMessage)
               -> f e
               -> Messages GhcMessage
foldPsMessages f = foldMap (singleMessage . fmap GhcPsMessage . f)

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
  DriverPsHeaderMessage :: !PsErrorDesc -> ![GhcHint] -> DriverMessage

  {-| DriverMissingHomeModules is a warning (controlled with -Wmissing-home-modules) that
      arises when running GHC in --make mode when some modules needed for compilation
      are not included on the command line. For example, if A imports B, `ghc --make
      A.hs` will cause this warning, while `ghc --make A.hs B.hs` will not.

      Useful for cabal to ensure GHC won't pick up modules listed neither in
      'exposed-modules' nor in 'other-modules'.

      Test case: warnings/should_compile/MissingMod

  -}
  DriverMissingHomeModules :: [ModuleName] -> !BuildingCabalPackage -> DriverMessage

  {-| DriverUnusedPackages occurs when when package is requested on command line,
      but was never needed during compilation. Activated by -Wunused-packages.

     Test cases: warnings/should_compile/UnusedPackages
  -}
  DriverUnusedPackages :: [PackageArg] -> DriverMessage

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
