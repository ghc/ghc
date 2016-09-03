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
import PackageConfig
import Packages
import Platform
import SysTools
import Module

import Data.Version

import System.FilePath
import System.Directory (findFile)

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
getTargetArchitecture :: String
getTargetArchitecture 
  = case TARGET_ARCH of
      "x86"    -> "x86"
      "x86_64" -> "amd64"
      _        -> error "Unknown TARGET_ARCH"

-- | Generate manifest file for the given configuration
-- TODO: Secure the dependencies with a SHA1 hash so we only
--       load genuine Haskell libraries.
generateManifest :: ManifestFile -> String
generateManifest manifest
  = unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
            , "<assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\">"
            , "  <assemblyIdentity version=\"" ++ version manifest ++ "\""
            , "                    processorArchitecture=\"" ++ architecture manifest ++ "\""
            , "                    name=\"" ++ name manifest ++ "\""
            , "                    type=\"win32\"/>"

            -- Generate a security context for binary compiles
            , if not $ isApplication manifest
                 then ""
                 else unlines 
                       [ ""
                        , "  <trustInfo xmlns=\"urn:schemas-microsoft-com:asm.v3\">"
                        , "    <security>"
                        , "      <requestedPrivileges>"
                        , "        <requestedExecutionLevel level=\"asInvoker\" uiAccess=\"false\"/>"
                        , "        </requestedPrivileges>"
                        , "       </security>"
                        , "  </trustInfo>"
                        ]

            , if null $ dependencies manifest
                 then ""
                 else unlines
                       [ ""
                       -- Generate dependencies
                       , unlines $ map (\dep -> unlines 
                           [ "  <dependency>"
                           , "   <dependentAssembly>"
                           , "    <assemblyIdentity name=\"" ++ (dropExtension $ fullname dep) ++ "\""
                           , "                      version=\"" ++ version dep ++ "\""
                           , "                      type=\"win32\""
                           , "                      processorArchitecture=\"" ++ architecture dep ++ "\"/>"
                           , "   </dependentAssembly>"
                           , "  </dependency>"
                           ]) (dependencies manifest)

                       , ""
                       -- Generate dependency names. This is controlled directly by the SxS names.
                       ,  unlines $ map (\dep ->  
                           "  <file name=\"" ++ (takeFileName $ fullname dep) ++ "\" />"
                           ) (dependencies manifest)
                       
                       ]
            , "</assembly>"
            ]

createManifestDefinition :: DynFlags -> [PackageConfig] -> FilePath -> IO ManifestFile
createManifestDefinition dflags pkgs assembly = do
  let isBinary = case ghcLink dflags of
                    LinkBinary -> True
                    LinkDynLib -> False
                    _          -> error "Link mode nor supported for assembly generation"
  deps <- if WayDyn `elem` ways dflags 
             then genDependencies pkgs
             else return []
  return ManifestFile { name          = maybe (dropExtension $ takeFileName assembly) id (sharedLibABIName    dflags)
                      , version       = maybe "1.0.0.0"  id (sharedLibABIVersion dflags)
                      , architecture  = getTargetArchitecture
                      , isApplication = isBinary
                      , fullname      = ""
                      , dependencies  = deps
                      }
     where genDependencies :: [PackageConfig] -> IO [ManifestFile]
           genDependencies []       = return []
           genDependencies (dep:xs) = do              
              let fullPkgName = if packageConfigId dep == rtsUnitId
                                   then "libHS"
                                        ++ packageNameString dep
                                       <.> ".dll"
                                   else "libHS"
                                        ++ packageNameString dep
                                        ++ "-"
                                        ++ (showVersion $ packageVersion dep)
                                        ++ "-ghc"
                                        ++ projectVersion dflags
                                       <.> ".dll"
              let modPath = if packageConfigId dep == rtsUnitId
                               then \base -> base </> "rts"
                                                  </> ("ghc" ++ projectVersion dflags)
                                                  </> mkBuildTag (filterRtsWays $ ways dflags)
                               else id

              fullPkgPath <- findFile (map modPath $ libraryDirs dep) fullPkgName
              print (map modPath $ libraryDirs dep)
              print fullPkgPath
              let manifest = ManifestFile { name          = packageNameString dep
                                          , version       = showVersion $ packageVersion dep
                                          , architecture  = getTargetArchitecture
                                          , isApplication = False
                                          , fullname      = case sxsResolveMode dflags of
                                                               SxSRelative -> "Get abs path from commandline"
                                                               SxSAbsolute -> maybe (packageNameString dep) id fullPkgPath
                                                               SxSCache    -> fullPkgName
                                          , dependencies  = []
                                          }
              rest <- genDependencies xs
              return (manifest : rest)

-- | Generate the appropriate Manifest file for program inclusion.
mkManifest
   :: DynFlags
   -> [PackageConfig]                   -- dependencies of this link object
   -> FilePath                          -- filename of executable
   -> IO [FilePath]                     -- extra objects to embed, maybe
mkManifest dflags pkgs assembly
 | platformOS (targetPlatform dflags) == OSMinGW32 &&
   gopt Opt_GenManifest dflags
    = do let manifest_filename = assembly <.> "manifest"

         manifest_def <- createManifestDefinition dflags pkgs assembly

         writeFile manifest_filename
             $ generateManifest manifest_def

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

             return [rc_obj_filename]
 | otherwise = return []