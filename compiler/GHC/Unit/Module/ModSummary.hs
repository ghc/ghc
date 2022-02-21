{-# LANGUAGE TupleSections #-}

-- | A ModSummary is a node in the compilation manager's dependency graph
-- (ModuleGraph)
module GHC.Unit.Module.ModSummary
   ( ModSummary (..)
   , ms_unitid
   , ms_installed_mod
   , ms_mod_name
   , ms_imps
   , ms_plugin_imps
   , ms_mnwib
   , ms_home_srcimps
   , ms_home_imps
   , msHiFilePath
   , msDynHiFilePath
   , msHsFilePath
   , msObjFilePath
   , msDynObjFilePath
   , msDeps
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
import GHC.Types.PkgQual

import GHC.Data.Maybe
import GHC.Data.StringBuffer ( StringBuffer )

import GHC.Utils.Fingerprint
import GHC.Utils.Outputable

import Data.Time


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
        ms_hs_hash      :: Fingerprint,
          -- ^ Content hash of source file
        ms_obj_date     :: Maybe UTCTime,
          -- ^ Timestamp of object, if we have one
        ms_dyn_obj_date     :: !(Maybe UTCTime),
          -- ^ Timestamp of dynamic object, if we have one
        ms_iface_date   :: Maybe UTCTime,
          -- ^ Timestamp of hi file, if we have one
          -- See Note [When source is considered modified] and #9243
        ms_hie_date   :: Maybe UTCTime,
          -- ^ Timestamp of hie file, if we have one
        ms_srcimps      :: [(PkgQual, Located ModuleName)], -- FIXME: source imports are never from an external package, why do we allow PkgQual?
          -- ^ Source imports of the module
        ms_textual_imps :: [(PkgQual, Located ModuleName)],
          -- ^ Non-source imports of the module from the module *text*
        ms_ghc_prim_import :: !Bool,
          -- ^ Whether the special module GHC.Prim was imported explicitliy
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

ms_unitid :: ModSummary -> UnitId
ms_unitid = toUnitId . moduleUnit . ms_mod

ms_installed_mod :: ModSummary -> InstalledModule
ms_installed_mod = fst . getModuleInstantiation . ms_mod

ms_mod_name :: ModSummary -> ModuleName
ms_mod_name = moduleName . ms_mod

-- | Textual imports, plus plugin imports but not SOURCE imports.
ms_imps :: ModSummary -> [(PkgQual, Located ModuleName)]
ms_imps ms = ms_textual_imps ms ++ ms_plugin_imps ms

-- | Plugin imports
ms_plugin_imps :: ModSummary -> [(PkgQual, Located ModuleName)]
ms_plugin_imps ms = map ((NoPkgQual,) . noLoc) (pluginModNames (ms_hspp_opts ms))

-- | All of the (possibly) home module imports from the given list that is to
-- say, each of these module names could be a home import if an appropriately
-- named file existed.  (This is in contrast to package qualified imports, which
-- are guaranteed not to be home imports.)
home_imps :: [(PkgQual, Located ModuleName)] -> [(PkgQual, Located ModuleName)]
home_imps imps = filter (maybe_home . fst) imps
  where maybe_home NoPkgQual    = True
        maybe_home (ThisPkg _)  = True
        maybe_home (OtherPkg _) = False

-- | Like 'ms_home_imps', but for SOURCE imports.
ms_home_srcimps :: ModSummary -> ([Located ModuleName])
-- [] here because source imports can only refer to the current package.
ms_home_srcimps = map snd . home_imps . ms_srcimps

-- | All of the (possibly) home module imports from a
-- 'ModSummary'; that is to say, each of these module names
-- could be a home import if an appropriately named file
-- existed.  (This is in contrast to package qualified
-- imports, which are guaranteed not to be home imports.)
ms_home_imps :: ModSummary -> ([(PkgQual, Located ModuleName)])
ms_home_imps = home_imps . ms_imps

-- The ModLocation contains both the original source filename and the
-- filename of the cleaned-up source file after all preprocessing has been
-- done.  The point is that the summariser will have to cpp/unlit/whatever
-- all files anyway, and there's no point in doing this twice -- just
-- park the result in a temp file, put the name of it in the location,
-- and let @compile@ read from that file on the way back up.

-- The ModLocation is stable over successive up-sweeps in GHCi, wheres
-- the ms_hs_hash and imports can, of course, change

msHsFilePath, msDynHiFilePath, msHiFilePath, msObjFilePath, msDynObjFilePath :: ModSummary -> FilePath
msHsFilePath  ms = expectJust "msHsFilePath" (ml_hs_file  (ms_location ms))
msHiFilePath  ms = ml_hi_file  (ms_location ms)
msDynHiFilePath ms = ml_dyn_hi_file (ms_location ms)
msObjFilePath ms = ml_obj_file (ms_location ms)
msDynObjFilePath ms = ml_dyn_obj_file (ms_location ms)

-- | Did this 'ModSummary' originate from a hs-boot file?
isBootSummary :: ModSummary -> IsBootInterface
isBootSummary ms = if ms_hsc_src ms == HsBootFile then IsBoot else NotBoot

ms_mnwib :: ModSummary -> ModuleNameWithIsBoot
ms_mnwib ms = GWIB (ms_mod_name ms) (isBootSummary ms)

-- | Returns the dependencies of the ModSummary s.
msDeps :: ModSummary -> ([(PkgQual, GenWithIsBoot (Located ModuleName))])
msDeps s =
           [ (NoPkgQual, d)
           | m <- ms_home_srcimps s
           , d <- [ GWIB { gwib_mod = m, gwib_isBoot = IsBoot }
                  ]
           ]
        ++ [ (pkg, (GWIB { gwib_mod = m, gwib_isBoot = NotBoot }))
           | (pkg, m) <- ms_imps s
           ]

instance Outputable ModSummary where
   ppr ms
      = sep [text "ModSummary {",
             nest 3 (sep [text "ms_hs_hash = " <> text (show (ms_hs_hash ms)),
                          text "ms_mod =" <+> ppr (ms_mod ms)
                                <> text (hscSourceString (ms_hsc_src ms)) <> comma,
                          text "unit =" <+> ppr (ms_unitid ms),
                          text "ms_textual_imps =" <+> ppr (ms_textual_imps ms),
                          text "ms_srcimps =" <+> ppr (ms_srcimps ms)]),
             char '}'
            ]

-- | Find the first target in the provided list which matches the specified
-- 'ModSummary'.
findTarget :: ModSummary -> [Target] -> Maybe Target
findTarget ms ts =
  case filter (matches ms) ts of
        []    -> Nothing
        (t:_) -> Just t
  where
    summary `matches` Target { targetId = TargetModule m, targetUnitId = unitId }
        = ms_mod_name summary == m && ms_unitid summary == unitId
    summary `matches` Target { targetId = TargetFile f _, targetUnitId = unitid }
        | Just f' <- ml_hs_file (ms_location summary)
        = f == f'  && ms_unitid summary == unitid
    _ `matches` _
        = False


