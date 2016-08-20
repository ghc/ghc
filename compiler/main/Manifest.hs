{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- GHC Windows Manifest generator
--
-- (c) The University of Glasgow 2016
--
-----------------------------------------------------------------------------

module Manifest (
   mkManifest
  ) where
  
import DynFlags
import Platform
import SysTools

import System.Directory
import System.FilePath
import System.Info

#include "ghcplatform.h"

-- | Manifest file required fields
data ManifestFile = ManifestFile { name          :: String
                                 , version       :: String
                                 , architecture  :: String
                                 , dependencies  :: [ManifestFile]
                                 , isApplication :: Bool
                                 , fullname      :: String
                                 }

-- | Translate TARGET_ARCH into something that Windows manifests expect                                 
getProcessorArchitecture :: String
getProcessorArchitecture 
  = case TARGET_ARCH of
      "x86"    -> "x86"
      "x86_64" -> "amd64"
      _        -> error "Unknown TARGET_ARCH"

-- | Generate manifest file for the given configuration
-- TODO: Secure the dependencies with a SHA1 hash so we only
--       load genuine Haskell libraries.
generateManifest :: ManifestFile -> String
generateManifest manifest | isApplication manifest
  = unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
            , "<assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\">"
            , "  <assemblyIdentity version=\"" ++ version manifest ++ "\""
            , "                    processorArchitecture=\"" ++ architecture manifest ++ "\""
            , "                    name=\"" ++ name manifest ++ "\""
            , "                    type=\"win32\"/>"
            , ""
            , "  <trustInfo xmlns=\"urn:schemas-microsoft-com:asm.v3\">"
            , "    <security>"
            , "      <requestedPrivileges>"
            , "        <requestedExecutionLevel level=\"asInvoker\" uiAccess=\"false\"/>"
            , "        </requestedPrivileges>"
            , "       </security>"
            , "  </trustInfo>"
            , ""
            , "  <dependency>"
            , "   <dependentAssembly>"
            , unlines $ map (\dep -> unlines 
                [ "    <assemblyIdentity name=\"" ++ name dep ++ "\""
                , "                      version=\"" ++ version dep ++ "\""
                , "                      type=\"win32\""
                , "                      processorArchitecture=\"" ++ architecture dep ++ "\"/>"
                ])
            , "   </dependentAssembly>"
            , "  </dependency>"
            , ""
            ,  unlines $ map (\dep -> unlines 
                [ "  <file name=\"" ++ fullname dep ++ "\" />"
                ])
            , "</assembly>"
            ]
generateManifest manifest
  = unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
            , "  <assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\">"
            , "  <trustInfo xmlns=\"urn:schemas-microsoft-com:asm.v3\">"
            , "    <security>"
            , "      <requestedPrivileges>"
            , "        <requestedExecutionLevel level=\"asInvoker\" uiAccess=\"false\"/>"
            , "        </requestedPrivileges>"
            , "       </security>"
            , "  </trustInfo>"
            , "</assembly>"
            ]
      
-- | Generate the appropriate Manifest file for program inclusion.
mkManifest
   :: DynFlags
   -> FilePath                          -- filename of executable
   -> IO [FilePath]                     -- extra objects to embed, maybe
mkManifest dflags assembly
 | platformOS (targetPlatform dflags) == OSMinGW32 &&
   gopt Opt_GenManifest dflags
    = do let manifest_filename = assembly <.> "manifest"

         writeFile manifest_filename $ generateManifest manifest

         -- Windows will find the manifest file if it is named
         -- foo.exe.manifest. However, for extra robustness, and so that
         -- we can move the binary around, we can embed the manifest in
         -- the binary itself using windres:
         if not (gopt Opt_EmbedManifest dflags) 
         then return [] 
         else do
             rc_filename <- newTempName dflags "rc"
             rc_obj_filename <- newTempName dflags (objectSuf dflags)

             writeFile rc_filename $
                 "1 24 MOVEABLE PURE " ++ show manifest_filename ++ "\n"
                   -- magic numbers :-)
                   -- show is a bit hackish above, but we need to escape the
                   -- backslashes in the path.

             runWindres dflags $ map SysTools.Option $
                   ["--input="++rc_filename,
                    "--output="++rc_obj_filename,
                    "--output-format=coff"]
                   -- no FileOptions here: windres doesn't like seeing
                   -- backslashes, apparently

             removeFile manifest_filename

             return [rc_obj_filename]
 | otherwise = return []