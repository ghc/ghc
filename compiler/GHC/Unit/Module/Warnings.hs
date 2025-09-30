{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- For Anno (WarningTxt (GhcPass pass))

-- | Warnings for a module
module GHC.Unit.Module.Warnings
   ( WarningCategory(..)
   , mkWarningCategory
   , defaultWarningCategory
   , validWarningCategory
   , InWarningCategory(..)
   , fromWarningCategory

   , WarningCategorySet
   , emptyWarningCategorySet
   , completeWarningCategorySet
   , nullWarningCategorySet
   , elemWarningCategorySet
   , insertWarningCategorySet
   , deleteWarningCategorySet

   , Warnings (..)
   , WarningTxt (..)
   , LWarningTxt
   , DeclWarnOccNames
   , ExportWarnNames
   , warningTxtCategory
   , warningTxtMessage
   , warningTxtSame
   , pprWarningTxtForMsg
   , emptyWarn
   , mkIfaceDeclWarnCache
   , mkIfaceExportWarnCache
   , emptyIfaceWarnCache
   , insertWarnDecls
   , insertWarnExports
   )
where

import GHC.Prelude

import GHC.Hs.Doc
import GHC.Hs.Extension
import GHC.Parser.Annotation
import GHC.Types.Name (Name)
import GHC.Types.Name.Env
import GHC.Types.Name.Occurrence
import GHC.Utils.Outputable

import Language.Haskell.Syntax.Extension
import qualified Language.Haskell.Textual.Source as Source
import Language.Haskell.Textual.Location
import Language.Haskell.Textual.Warning

{-
Note [Warning categories]
~~~~~~~~~~~~~~~~~~~~~~~~~
See GHC Proposal 541 for the design of the warning categories feature:
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0541-warning-pragmas-with-categories.rst

A WARNING pragma may be annotated with a category such as "x-partial" written
after the 'in' keyword, like this:

    {-# WARNING in "x-partial" head "This function is partial..." #-}

This is represented by the 'Maybe (Located WarningCategory)' field in
'WarningTxt'.  The parser will accept an arbitrary string as the category name,
then the renamer (in 'rnWarningTxt') will check it contains only valid
characters, so we can generate a nicer error message than a parse error.

The corresponding warnings can then be controlled with the -Wx-partial,
-Wno-x-partial, -Werror=x-partial and -Wwarn=x-partial flags.  Such a flag is
distinguished from an 'unrecognisedWarning' by the flag parser testing
'validWarningCategory'.  The 'x-' prefix means we can still usually report an
unrecognised warning where the user has made a mistake.

A DEPRECATED pragma may not have a user-defined category, and is always treated
as belonging to the special category 'deprecations'.  Similarly, a WARNING
pragma without a category belongs to the 'deprecations' category.
Thus the '-Wdeprecations' flag will enable all of the following:

    {-# WARNING in "deprecations" foo "This function is deprecated..." #-}
    {-# WARNING foo "This function is deprecated..." #-}
    {-# DEPRECATED foo "This function is deprecated..." #-}

The '-Wwarnings-deprecations' flag is supported for backwards compatibility
purposes as being equivalent to '-Wdeprecations'.

The '-Wextended-warnings' warning group collects together all warnings with
user-defined categories, so they can be enabled or disabled
collectively. Moreover they are treated as being part of other warning groups
such as '-Wdefault' (see 'warningGroupIncludesExtendedWarnings').

'DynFlags' and 'DiagOpts' each contain a set of enabled and a set of fatal
warning categories, just as they do for the finite enumeration of 'WarningFlag's
built in to GHC.  These are represented as 'WarningCategorySet's to allow for
the possibility of them being infinite.

-}

type LWarningTxt pass = XRec pass (WarningTxt pass)

type instance Anno (WarningTxt (GhcPass pass)) = SrcSpanAnnP

pprWarningTxtForMsg :: WarningTxt p -> SDoc
pprWarningTxtForMsg warn =
  let pprWarn = doubleQuotes . vcat . map (ppr . Source.sl_fs . hsDocString . unLoc)
  in  case warn of
        DeprecatedTxt _ ds -> text "Deprecated:" <+>
                              pprWarn ds
        WarningTxt  _ _ ws -> pprWarn ws

-- | Warning information from a module
data Warnings pass
  = WarnSome (DeclWarnOccNames pass) -- ^ Names deprecated (may be empty)
             (ExportWarnNames pass)  -- ^ Exports deprecated (may be empty)
  | WarnAll (WarningTxt pass)        -- ^ Whole module deprecated

     -- For the module-specific names only an OccName is needed because
     --    (1) a deprecation always applies to a binding
     --        defined in the module in which the deprecation appears.
     --    (2) deprecations are only reported outside the defining module.
     --        this is important because, otherwise, if we saw something like
     --
     --        {-# DEPRECATED f "" #-}
     --        f = ...
     --        h = f
     --        g = let f = undefined in f
     --
     --        we'd need more information than an OccName to know to say something
     --        about the use of f in h but not the use of the locally bound f in g
     --
     --        however, because we only report about deprecations from the outside,
     --        and a module can only export one value called f,
     --        an OccName suffices.
     --
     --        this is in contrast with fixity declarations, where we need to map
     --        a Name to its fixity declaration.
     --
     -- For export deprecations we need to know where the symbol comes from, since
     -- we need to be able to check if the deprecated export that was imported is
     -- the same thing as imported by another import, which would not trigger
     -- a deprecation message.

-- | Deprecated declarations
type DeclWarnOccNames pass = [(OccName, WarningTxt pass)]

-- | Names that are deprecated as exports
type ExportWarnNames pass = [(Name, WarningTxt pass)]

deriving instance Eq (IdP pass) => Eq (Warnings pass)

emptyWarn :: Warnings p
emptyWarn = WarnSome [] []

-- | Constructs the cache for the 'mi_decl_warn_fn' field of a 'ModIface'
mkIfaceDeclWarnCache :: Warnings p -> OccName -> Maybe (WarningTxt p)
mkIfaceDeclWarnCache (WarnAll t) = \_ -> Just t
mkIfaceDeclWarnCache (WarnSome vs _) = lookupOccEnv (mkOccEnv vs)

-- | Constructs the cache for the 'mi_export_warn_fn' field of a 'ModIface'
mkIfaceExportWarnCache :: Warnings p -> Name -> Maybe (WarningTxt p)
mkIfaceExportWarnCache (WarnAll _) = const Nothing -- We do not want a double report of the module deprecation
mkIfaceExportWarnCache (WarnSome _ ds) = lookupNameEnv (mkNameEnv ds)

emptyIfaceWarnCache :: name -> Maybe (WarningTxt p)
emptyIfaceWarnCache _ = Nothing

insertWarnDecls :: Warnings p                -- ^ Existing warnings
                -> [(OccName, WarningTxt p)] -- ^ New declaration deprecations
                -> Warnings p                -- ^ Updated warnings
insertWarnDecls ws@(WarnAll _) _        = ws
insertWarnDecls (WarnSome wns wes) wns' = WarnSome (wns ++ wns') wes

insertWarnExports :: Warnings p             -- ^ Existing warnings
                  -> [(Name, WarningTxt p)] -- ^ New export deprecations
                  -> Warnings p             -- ^ Updated warnings
insertWarnExports ws@(WarnAll _) _ = ws
insertWarnExports (WarnSome wns wes) wes' = WarnSome wns (wes ++ wes')
