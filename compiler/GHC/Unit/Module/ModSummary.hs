{-# LANGUAGE LambdaCase #-}

-- | A ModSummary is a node in the compilation manager's dependency graph
-- (ModuleGraph)
module GHC.Unit.Module.ModSummary
   ( ExtendedModSummary (..)
   , extendModSummaryNoDeps
   , ModSummary (..)
   , ms_installed_mod
   , ms_mod_name
   , ms_imps
   , ms_home_allimps
   , ms_home_srcimps
   , ms_home_imps
   , msHiFilePath
   , msHsFilePath
   , msObjFilePath
   , msDynObjFilePath
   , isBootSummary
   , findTarget
   )
where

import GHC.Prelude

import GHC.Hs

import GHC.Driver.Session

import GHC.Unit.Types
import GHC.Unit.Module

import GHC.Types.SourceFile ( HscSource(..), hscSourceString )
import GHC.Types.SrcLoc
import GHC.Types.Target

import GHC.Data.Maybe
import GHC.Data.FastString
import GHC.Data.StringBuffer ( StringBuffer )

import GHC.Utils.Outputable

import Data.Time

-- | Enrichment of 'ModSummary' with backpack dependencies
data ExtendedModSummary = ExtendedModSummary
  { emsModSummary :: {-# UNPACK #-} !ModSummary
  , emsInstantiatedUnits :: [InstantiatedUnit]
  -- ^ Extra backpack deps
  -- NB: This is sometimes left empty in situations where the instantiated units
  -- would not be used. See call sites of 'extendModSummaryNoDeps'.
  }

instance Outputable ExtendedModSummary where
  ppr = \case
    ExtendedModSummary ms bds -> ppr ms <+> ppr bds

extendModSummaryNoDeps :: ModSummary -> ExtendedModSummary
extendModSummaryNoDeps ms = ExtendedModSummary ms []

-- | Data for a module node in a 'ModuleGraph'. Module nodes of the module graph
-- are one of:
--
-- * A regular Haskell source module
-- * A hi-boot source module
--
data ModSummary
   = ModSummary {
        ms_mod          :: Module,
          -- ^ Identity of the module
        ms_hsc_src      :: HscSource,
          -- ^ The module source either plain Haskell, hs-boot, or hsig
        ms_location     :: ModLocation,
          -- ^ Location of the various files belonging to the module
        ms_hs_date      :: UTCTime,
          -- ^ Timestamp of source file
        ms_obj_date     :: Maybe UTCTime,
          -- ^ Timestamp of object, if we have one
        ms_iface_date   :: Maybe UTCTime,
          -- ^ Timestamp of hi file, if we *only* are typechecking (it is
          -- 'Nothing' otherwise.
          -- See Note [Recompilation checking in -fno-code mode] and #9243
        ms_hie_date   :: Maybe UTCTime,
          -- ^ Timestamp of hie file, if we have one
        ms_srcimps      :: [(Maybe FastString, Located ModuleName)],
          -- ^ Source imports of the module
        ms_textual_imps :: [(Maybe FastString, Located ModuleName)],
          -- ^ Non-source imports of the module from the module *text*
        ms_parsed_mod   :: Maybe HsParsedModule,
          -- ^ The parsed, nonrenamed source, if we have it.  This is also
          -- used to support "inline module syntax" in Backpack files.
        ms_hspp_file    :: FilePath,
          -- ^ Filename of preprocessed source file
        ms_hspp_opts    :: DynFlags,
          -- ^ Cached flags from @OPTIONS@, @INCLUDE@ and @LANGUAGE@
          -- pragmas in the modules source code
        ms_hspp_buf     :: Maybe StringBuffer
          -- ^ The actual preprocessed source, if we have it
     }

ms_installed_mod :: ModSummary -> InstalledModule
ms_installed_mod = fst . getModuleInstantiation . ms_mod

ms_mod_name :: ModSummary -> ModuleName
ms_mod_name = moduleName . ms_mod

ms_imps :: ModSummary -> [(Maybe FastString, Located ModuleName)]
ms_imps ms =
  ms_textual_imps ms ++
  map mk_additional_import (dynFlagDependencies (ms_hspp_opts ms))
  where
    mk_additional_import mod_nm = (Nothing, noLoc mod_nm)

home_imps :: [(Maybe FastString, Located ModuleName)] -> [Located ModuleName]
home_imps imps = [ lmodname |  (mb_pkg, lmodname) <- imps,
                                  isLocal mb_pkg ]
  where isLocal Nothing = True
        isLocal (Just pkg) | pkg == fsLit "this" = True -- "this" is special
        isLocal _ = False

ms_home_allimps :: ModSummary -> [ModuleName]
ms_home_allimps ms = map unLoc (ms_home_srcimps ms ++ ms_home_imps ms)

-- | Like 'ms_home_imps', but for SOURCE imports.
ms_home_srcimps :: ModSummary -> [Located ModuleName]
ms_home_srcimps = home_imps . ms_srcimps

-- | All of the (possibly) home module imports from a
-- 'ModSummary'; that is to say, each of these module names
-- could be a home import if an appropriately named file
-- existed.  (This is in contrast to package qualified
-- imports, which are guaranteed not to be home imports.)
ms_home_imps :: ModSummary -> [Located ModuleName]
ms_home_imps = home_imps . ms_imps

-- The ModLocation contains both the original source filename and the
-- filename of the cleaned-up source file after all preprocessing has been
-- done.  The point is that the summariser will have to cpp/unlit/whatever
-- all files anyway, and there's no point in doing this twice -- just
-- park the result in a temp file, put the name of it in the location,
-- and let @compile@ read from that file on the way back up.

-- The ModLocation is stable over successive up-sweeps in GHCi, wheres
-- the ms_hs_date and imports can, of course, change

msHsFilePath, msHiFilePath, msObjFilePath :: ModSummary -> FilePath
msHsFilePath  ms = expectJust "msHsFilePath" (ml_hs_file  (ms_location ms))
msHiFilePath  ms = ml_hi_file  (ms_location ms)
msObjFilePath ms = ml_obj_file (ms_location ms)

msDynObjFilePath :: ModSummary -> DynFlags -> FilePath
msDynObjFilePath ms dflags = dynamicOutputFile dflags (msObjFilePath ms)

-- | Did this 'ModSummary' originate from a hs-boot file?
isBootSummary :: ModSummary -> IsBootInterface
isBootSummary ms = if ms_hsc_src ms == HsBootFile then IsBoot else NotBoot

instance Outputable ModSummary where
   ppr ms
      = sep [text "ModSummary {",
             nest 3 (sep [text "ms_hs_date = " <> text (show (ms_hs_date ms)),
                          text "ms_mod =" <+> ppr (ms_mod ms)
                                <> text (hscSourceString (ms_hsc_src ms)) <> comma,
                          text "ms_textual_imps =" <+> ppr (ms_textual_imps ms),
                          text "ms_srcimps =" <+> ppr (ms_srcimps ms)]),
             char '}'
            ]

findTarget :: ModSummary -> [Target] -> Maybe Target
findTarget ms ts =
  case filter (matches ms) ts of
        []    -> Nothing
        (t:_) -> Just t
  where
    summary `matches` Target (TargetModule m) _ _
        = ms_mod_name summary == m
    summary `matches` Target (TargetFile f _) _ _
        | Just f' <- ml_hs_file (ms_location summary)
        = f == f'
    _ `matches` _
        = False


