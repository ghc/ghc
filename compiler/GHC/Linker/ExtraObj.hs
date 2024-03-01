-----------------------------------------------------------------------------
--
-- GHC Extra object linking code
--
-- (c) The GHC Team 2017
--
-----------------------------------------------------------------------------

module GHC.Linker.ExtraObj
   ( mkExtraObj
   , mkExtraObjToLinkIntoBinary
   , mkNoteObjsToLinkIntoBinary
   , checkLinkInfo
   , getLinkInfo
   , ghcLinkInfoSectionName
   , ghcLinkInfoNoteName
   , platformSupportsSavingLinkOpts
   , haveRtsOptsFlags
   )
where

import GHC.Prelude
import GHC.Platform

import GHC.Unit
import GHC.Unit.Env

import GHC.Utils.Asm
import GHC.Utils.Error
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Logger
import GHC.Utils.TmpFs

import GHC.Driver.Session
import GHC.Driver.Ppr

import qualified GHC.Data.ShortText as ST

import GHC.SysTools.Elf
import GHC.SysTools.Tasks
import GHC.Linker.Unit

import Control.Monad
import Data.Maybe

mkExtraObj :: Logger -> TmpFs -> DynFlags -> UnitState -> Suffix -> String -> IO FilePath
mkExtraObj logger tmpfs dflags unit_state extn xs
 = do cFile <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule extn
      oFile <- newTempName logger tmpfs (tmpDir dflags) TFL_GhcSession "o"
      writeFile cFile xs
      runCc Nothing logger tmpfs dflags
            ([Option        "-c",
              FileOption "" cFile,
              Option        "-o",
              FileOption "" oFile]
              ++ if extn /= "s"
                    then cOpts
                    else [])
      return oFile
    where
      -- Pass a different set of options to the C compiler depending one whether
      -- we're compiling C or assembler. When compiling C, we pass the usual
      -- set of include directories and PIC flags.
      cOpts = map Option (picCCOpts dflags)
                    ++ map (FileOption "-I" . ST.unpack)
                            (unitIncludeDirs $ unsafeLookupUnit unit_state rtsUnit)

-- When linking a binary, we need to create a C main() function that
-- starts everything off.  This used to be compiled statically as part
-- of the RTS, but that made it hard to change the -rtsopts setting,
-- so now we generate and compile a main() stub as part of every
-- binary and pass the -rtsopts setting directly to the RTS (#5373)
--
-- On Windows, when making a shared library we also may need a DllMain.
--
mkExtraObjToLinkIntoBinary :: Logger -> TmpFs -> DynFlags -> UnitState -> IO (Maybe FilePath)
mkExtraObjToLinkIntoBinary logger tmpfs dflags unit_state = do
  when (gopt Opt_NoHsMain dflags && haveRtsOptsFlags dflags) $
     logInfo logger $ withPprStyle defaultUserStyle
         (text "Warning: -rtsopts and -with-rtsopts have no effect with -no-hs-main." $$
          text "    Call hs_init_ghc() from your main() function to set these options.")

  case ghcLink dflags of
    -- Don't try to build the extra object if it is not needed.  Compiling the
    -- extra object assumes the presence of the RTS in the unit database
    -- (because the extra object imports Rts.h) but GHC's build system may try
    -- to build some helper programs before building and registering the RTS!
    -- See #18938 for an example where hp2ps failed to build because of a failed
    -- (unsafe) lookup for the RTS in the unit db.
    _ | gopt Opt_NoHsMain dflags
      -> return Nothing

    LinkDynLib
      | OSMinGW32 <- platformOS (targetPlatform dflags)
      -> mk_extra_obj dllMain

      | otherwise
      -> return Nothing

    _ -> mk_extra_obj exeMain

  where
    mk_extra_obj = fmap Just . mkExtraObj logger tmpfs dflags unit_state "c" . showSDoc dflags

    exeMain = vcat [
        text "#include <Rts.h>",
        text "extern StgClosure ZCMain_main_closure;",
        text "int main(int argc, char *argv[])",
        char '{',
        text " RtsConfig __conf = defaultRtsConfig;",
        text " __conf.rts_opts_enabled = "
            <> text (show (rtsOptsEnabled dflags)) <> semi,
        text " __conf.rts_opts_suggestions = "
            <> (if rtsOptsSuggestions dflags
                then text "true"
                else text "false") <> semi,
        text "__conf.keep_cafs = "
            <> (if gopt Opt_KeepCAFs dflags
                then text "true"
                else text "false") <> semi,
        case rtsOpts dflags of
            Nothing   -> Outputable.empty
            Just opts -> text "    __conf.rts_opts= " <>
                          text (show opts) <> semi,
        text " __conf.rts_hs_main = true;",
        text " return hs_main(argc,argv,&ZCMain_main_closure,__conf);",
        char '}',
        char '\n' -- final newline, to keep gcc happy
        ]

    dllMain = vcat [
        text "#include <Rts.h>",
        text "#include <windows.h>",
        text "#include <stdbool.h>",
        char '\n',
        text "bool",
        text "WINAPI",
        text "DllMain ( HINSTANCE hInstance STG_UNUSED",
        text "        , DWORD reason STG_UNUSED",
        text "        , LPVOID reserved STG_UNUSED",
        text "        )",
        text "{",
        text "  return true;",
        text "}",
        char '\n' -- final newline, to keep gcc happy
        ]

-- Write out the link info section into a new assembly file. Previously
-- this was included as inline assembly in the main.c file but this
-- is pretty fragile. gas gets upset trying to calculate relative offsets
-- that span the .note section (notably .text) when debug info is present
mkNoteObjsToLinkIntoBinary :: Logger -> TmpFs -> DynFlags -> UnitEnv -> [UnitId] -> IO [FilePath]
mkNoteObjsToLinkIntoBinary logger tmpfs dflags unit_env dep_packages = do
   link_info <- getLinkInfo dflags unit_env dep_packages

   if (platformSupportsSavingLinkOpts (platformOS platform ))
     then fmap (:[]) $ mkExtraObj logger tmpfs dflags unit_state "s" (showSDoc dflags (link_opts link_info))
     else return []

  where
    unit_state = ue_homeUnitState unit_env
    platform   = ue_platform unit_env
    link_opts info = hcat
        [ -- "link info" section (see Note [LinkInfo section])
          makeElfNote platform ghcLinkInfoSectionName ghcLinkInfoNoteName 0 info

        -- ALL generated assembly must have this section to disable
        -- executable stacks.  See also
        -- "GHC.CmmToAsm" for another instance
        -- where we need to do this.
        , if platformHasGnuNonexecStack platform
            then text ".section .note.GNU-stack,\"\","
                 <> sectionType platform "progbits" <> char '\n'
            else Outputable.empty
        ]

-- | Return the "link info" string
--
-- See Note [LinkInfo section]
getLinkInfo :: DynFlags -> UnitEnv -> [UnitId] -> IO String
getLinkInfo dflags unit_env dep_packages = do
    package_link_opts <- getUnitLinkOpts (ghcNameVersion dflags) (ways dflags) unit_env dep_packages
    pkg_frameworks <- if not (platformUsesFrameworks (ue_platform unit_env))
      then return []
      else do
         ps <- mayThrowUnitErr (preloadUnitsInfo' unit_env dep_packages)
         return (collectFrameworks ps)
    let link_info =
             ( package_link_opts
             , pkg_frameworks
             , rtsOpts dflags
             , rtsOptsEnabled dflags
             , gopt Opt_NoHsMain dflags
             , map showOpt (ldInputs dflags)
             , getOpts dflags opt_l
             )
    return (show link_info)

platformSupportsSavingLinkOpts :: OS -> Bool
platformSupportsSavingLinkOpts os
 | os == OSSolaris2 = False -- see #5382
 | otherwise        = osElfTarget os

-- See Note [LinkInfo section]
ghcLinkInfoSectionName :: String
ghcLinkInfoSectionName = ".debug-ghc-link-info"
  -- if we use the ".debug" prefix, then strip will strip it by default

-- Identifier for the note (see Note [LinkInfo section])
ghcLinkInfoNoteName :: String
ghcLinkInfoNoteName = "GHC link info"

-- Returns 'False' if it was, and we can avoid linking, because the
-- previous binary was linked with "the same options".
checkLinkInfo :: Logger -> DynFlags -> UnitEnv -> [UnitId] -> FilePath -> IO Bool
checkLinkInfo logger dflags unit_env pkg_deps exe_file
 | not (platformSupportsSavingLinkOpts (platformOS (ue_platform unit_env)))
 -- ToDo: Windows and OS X do not use the ELF binary format, so
 -- readelf does not work there.  We need to find another way to do
 -- this.
 = return False -- conservatively we should return True, but not
                -- linking in this case was the behaviour for a long
                -- time so we leave it as-is.
 | otherwise
 = do
   link_info <- getLinkInfo dflags unit_env pkg_deps
   debugTraceMsg logger 3 $ text ("Link info: " ++ link_info)
   m_exe_link_info <- readElfNoteAsString logger exe_file
                          ghcLinkInfoSectionName ghcLinkInfoNoteName
   let sameLinkInfo = (Just link_info == m_exe_link_info)
   debugTraceMsg logger 3 $ case m_exe_link_info of
     Nothing -> text "Exe link info: Not found"
     Just s
       | sameLinkInfo -> text ("Exe link info is the same")
       | otherwise    -> text ("Exe link info is different: " ++ s)
   return (not sameLinkInfo)

{- Note [LinkInfo section]
   ~~~~~~~~~~~~~~~~~~~~~~~

The "link info" is a string representing the parameters of the link. We save
this information in the binary, and the next time we link, if nothing else has
changed, we use the link info stored in the existing binary to decide whether
to re-link or not.

The "link info" string is stored in a ELF section called ".debug-ghc-link-info"
(see ghcLinkInfoSectionName) with the SHT_NOTE type.  For some time, it used to
not follow the specified record-based format (see #11022).

-}

haveRtsOptsFlags :: DynFlags -> Bool
haveRtsOptsFlags dflags =
        isJust (rtsOpts dflags) || case rtsOptsEnabled dflags of
                                       RtsOptsSafeOnly -> False
                                       _ -> True
