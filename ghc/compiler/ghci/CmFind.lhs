%
% (c) The University of Glasgow, 2000
%
\section[CmFind]{Module finder for GHCI}

\begin{code}
module CmFind ( Finder, newFinder )
where

#include "HsVersions.h"

import IO		( hPutStr, stderr )
import List		( maximumBy )
import Maybe		( catMaybes )
import Time		( ClockTime )
import Directory	( doesFileExist, getModificationTime )
import Outputable

import Module		( Module, ModuleName, ModuleKind(..), PackageName, 
			  mkModule, moduleNameUserString )
import CmStaticInfo	( Package(..), PackageConfigInfo(..) )
\end{code}

\begin{code}
type Finder = ModuleName -> IO (Maybe Module)

mkFinder :: [(ModuleName,PackageName,FilePath)] -> [FilePath] -> Finder
mkFinder pkg_ifaces home_dirs modnm
   = do found <- mkFinderX pkg_ifaces home_dirs modnm
        --putStrLn ("FINDER: request  = " ++ modnm ++ "\n" ++
        --          "FINDER: response = " ++ showSDoc (ppr found))
        return found


mkFinderX :: [(ModuleName,PackageName,FilePath)] -> [FilePath] -> Finder
mkFinderX pkg_ifaces home_dirs modnm
   -- If the module exists both as package and home, emit a warning
   -- and (arbitrarily) choose the user's one.
   = do home_maybe_found <- mapM (homeModuleExists modnm) home_dirs
                         :: IO [Maybe (Module, ClockTime)]
        case (in_package, catMaybes home_maybe_found) of
           ([], []) 
              -> return Nothing
           ([], locs_n_times@(_:_))
              -> return (Just (homeMod locs_n_times))
           ((pkgname,path):_, [])
              -> return (Just (mkModule modnm (InPackage pkgname)))
           (packages, locs_n_times)
              -> do hPutStr stderr ( "GHCI: warning: module `" 
                                     ++ moduleNameUserString modnm
                                     ++ "' appears as both a home and package module\n")
                    return (Just (homeMod locs_n_times))
     where
        in_package 
           = [(pkgname,path) | (modname,pkgname,path) <- pkg_ifaces, 
                               modname == modnm]
        homeMod :: [(Module, ClockTime)] -> Module
        homeMod locs_n_times
           = fst (maximumBy (\lt1 lt2 -> if snd lt1 > snd lt2 then lt1 else lt2)
                            locs_n_times)
        

-- See if a .hs or (.hi, .o) pair exist on the given path,
-- and return a Module for whichever is younger
homeModuleExists :: ModuleName -> FilePath -> IO (Maybe (Module, ClockTime))
homeModuleExists modname path
   = do m_ths <- maybeTime nm_hs
        m_thi <- maybeTime nm_hi
        m_to  <- maybeTime nm_o
        return (
           case (m_ths, m_thi, m_to) of
              (Just ths, Just thi, Just to)
                 |  thi >= ths && to >= ths -> object thi to
                 |  otherwise               -> source ths
              (Just ths, _, _)              -> source ths
              (Nothing, Just thi, Just to)  -> object thi to
              (Nothing, _, _)               -> Nothing
           )
     where
        object thi to = Just (mkModule modname (ObjectCode nm_o nm_hi), 
                              max thi to)
        source ths    = Just (mkModule modname (SourceOnly nm_hs), 
                              ths)
        nm = path ++ "/" ++ moduleNameUserString modname
        nm_hs = nm ++ ".hs"
        nm_hi = nm ++ ".hi"
        nm_o  = nm ++ ".o"

        maybeTime :: String -> IO (Maybe ClockTime)
        maybeTime f
           = do -- putStrLn ("maybeTime: " ++ f)
                exists <- doesFileExist f
                if not exists 
                 then do -- putStrLn " ... no"
                         return Nothing
                 else do tm <- getModificationTime f
                         -- putStrLn (" ... " ++ show tm)
                         return (Just tm)



newFinder :: FilePath{-temp debugging hack-}
          -> PackageConfigInfo -> IO Finder
newFinder path pci
   = return (mkFinder (pci_modtable pci) [path])

\end{code}
