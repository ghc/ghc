-----------------------------------------------------------------------------
--
-- GHC Extra object linking code
--
-- (c) The GHC Team 2017
--
-----------------------------------------------------------------------------

module SysTools.ExtraObj (
  mkExtraObj, mkExtraObjToLinkIntoBinary, mkNoteObjsToLinkIntoBinary,
  checkLinkInfo, getLinkInfo, getCompilerInfo,
  ghcLinkInfoSectionName, ghcLinkInfoNoteName, platformSupportsSavingLinkOpts,
  haveRtsOptsFlags
) where

import AsmUtils
import ErrUtils
import DynFlags
import Packages
import Platform
import Outputable
import SrcLoc           ( noSrcSpan )
import Module
import Elf
import Util
import GhcPrelude

import Control.Monad
import Data.Maybe

import Control.Monad.IO.Class

import FileCleanup
import SysTools.Tasks
import SysTools.Info

mkExtraObj :: DynFlags -> Suffix -> String -> IO FilePath
mkExtraObj dflags extn xs
 = do cFile <- newTempName dflags TFL_CurrentModule extn
      oFile <- newTempName dflags TFL_GhcSession "o"
      writeFile cFile xs
      ccInfo <- liftIO $ getCompilerInfo dflags
      runCc dflags
            ([Option        "-c",
              FileOption "" cFile,
              Option        "-o",
              FileOption "" oFile]
              ++ if extn /= "s"
                    then cOpts
                    else asmOpts ccInfo)
      return oFile
    where
      -- Pass a different set of options to the C compiler depending one whether
      -- we're compiling C or assembler. When compiling C, we pass the usual
      -- set of include directories and PIC flags.
      cOpts = map Option (picCCOpts dflags)
                    ++ map (FileOption "-I")
                            (includeDirs $ getPackageDetails dflags rtsUnitId)

      -- When compiling assembler code, we drop the usual C options, and if the
      -- compiler is Clang, we add an extra argument to tell Clang to ignore
      -- unused command line options. See trac #11684.
      asmOpts ccInfo =
            if any (ccInfo ==) [Clang, AppleClang, AppleClang51]
                then [Option "-Qunused-arguments"]
                else []

-- When linking a binary, we need to create a C main() function that
-- starts everything off.  This used to be compiled statically as part
-- of the RTS, but that made it hard to change the -rtsopts setting,
-- so now we generate and compile a main() stub as part of every
-- binary and pass the -rtsopts setting directly to the RTS (#5373)
--
-- On Windows, when making a shared library we also may need a DllMain.
--
mkExtraObjToLinkIntoBinary :: DynFlags -> IO FilePath
mkExtraObjToLinkIntoBinary dflags = do
  when (gopt Opt_NoHsMain dflags && haveRtsOptsFlags dflags) $ do
     putLogMsg dflags NoReason SevInfo noSrcSpan
         (defaultUserStyle dflags)
         (text "Warning: -rtsopts and -with-rtsopts have no effect with -no-hs-main." $$
          text "    Call hs_init_ghc() from your main() function to set these options.")

  mkExtraObj dflags "c" (showSDoc dflags main)
  where
    main
      | gopt Opt_NoHsMain dflags = Outputable.empty
      | otherwise
          = case ghcLink dflags of
                  LinkDynLib -> if platformOS (targetPlatform dflags) == OSMinGW32
                                    then dllMain
                                    else Outputable.empty
                  _                      -> exeMain

    exeMain = vcat [
        text "#include \"Rts.h\"",
        text "extern StgClosure ZCMain_main_closure;",
        text "int main(int argc, char *argv[])",
        char '{',
        text " RtsConfig __conf = defaultRtsConfig;",
        text " __conf.rts_opts_enabled = "
            <> text (show (rtsOptsEnabled dflags)) <> semi,
        text " __conf.rts_opts_suggestions = "
            <> text (if rtsOptsSuggestions dflags
                        then "true"
                        else "false") <> semi,
        text "__conf.keep_cafs = "
            <> text (if gopt Opt_KeepCAFs dflags
                       then "true"
                       else "false") <> semi,
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
        text "#include \"Rts.h\"",
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
mkNoteObjsToLinkIntoBinary :: DynFlags -> [InstalledUnitId] -> IO [FilePath]
mkNoteObjsToLinkIntoBinary dflags dep_packages = do
   link_info <- getLinkInfo dflags dep_packages

   if (platformSupportsSavingLinkOpts (platformOS (targetPlatform dflags)))
     then fmap (:[]) $ mkExtraObj dflags "s" (showSDoc dflags (link_opts link_info))
     else return []

  where
    link_opts info = hcat [
      -- "link info" section (see Note [LinkInfo section])
      makeElfNote ghcLinkInfoSectionName ghcLinkInfoNoteName 0 info,

      -- ALL generated assembly must have this section to disable
      -- executable stacks.  See also
      -- compiler/nativeGen/AsmCodeGen.hs for another instance
      -- where we need to do this.
      if platformHasGnuNonexecStack (targetPlatform dflags)
        then text ".section .note.GNU-stack,\"\","
             <> sectionType "progbits" <> char '\n'
        else Outputable.empty
      ]

-- | Return the "link info" string
--
-- See Note [LinkInfo section]
getLinkInfo :: DynFlags -> [InstalledUnitId] -> IO String
getLinkInfo dflags dep_packages = do
   package_link_opts <- getPackageLinkOpts dflags dep_packages
   pkg_frameworks <- if platformUsesFrameworks (targetPlatform dflags)
                     then getPackageFrameworks dflags dep_packages
                     else return []
   let extra_ld_inputs = ldInputs dflags
   let
      link_info = (package_link_opts,
                   pkg_frameworks,
                   rtsOpts dflags,
                   rtsOptsEnabled dflags,
                   gopt Opt_NoHsMain dflags,
                   map showOpt extra_ld_inputs,
                   getOpts dflags opt_l)
   --
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
checkLinkInfo :: DynFlags -> [InstalledUnitId] -> FilePath -> IO Bool
checkLinkInfo dflags pkg_deps exe_file
 | not (platformSupportsSavingLinkOpts (platformOS (targetPlatform dflags)))
 -- ToDo: Windows and OS X do not use the ELF binary format, so
 -- readelf does not work there.  We need to find another way to do
 -- this.
 = return False -- conservatively we should return True, but not
                -- linking in this case was the behaviour for a long
                -- time so we leave it as-is.
 | otherwise
 = do
   link_info <- getLinkInfo dflags pkg_deps
   debugTraceMsg dflags 3 $ text ("Link info: " ++ link_info)
   m_exe_link_info <- readElfNoteAsString dflags exe_file
                          ghcLinkInfoSectionName ghcLinkInfoNoteName
   let sameLinkInfo = (Just link_info == m_exe_link_info)
   debugTraceMsg dflags 3 $ case m_exe_link_info of
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
