%
% (c) The University of Glasgow, 2000
%
\section[CmFind]{Module finder for GHCI}

\begin{code}
module CmFind ( ModLocation(..), ml_modname, isPackageLoc,
		Finder, newFinder )
where

#include "HsVersions.h"

import IO		( hPutStr, stderr )
import List		( maximumBy )
import Maybe		( catMaybes )
import Time		( ClockTime )
import Directory	( doesFileExist, getModificationTime )
import Outputable

import Module		( Module, ModuleName, PackageName )
import CmStaticInfo	( PCI(..), Package(..) )
\end{code}

\begin{code}
-- make a product type, with Maybe return --> Module,lhs
data ModLocation 
   = SourceOnly ModuleName Path        -- .hs
   | ObjectCode ModuleName Path Path   -- .o, .hi
   | InPackage  ModuleName PackageName
   | NotFound

instance Outputable ModLocation where
   ppr (SourceOnly nm path_hs) 
      = hsep [text "SourceOnly", text (show nm), text (show path_hs)]
   ppr (ObjectCode nm path_o path_hi)
      = hsep [text "ObjectCode", text (show nm), 
                                 text (show path_o), text (show path_hi)]
   ppr (InPackage nm pkgname)
      = hsep [text "InPackage", text (show nm), text (show pkgname)]



type Finder = ModuleName -> IO ModLocation

ml_modname (SourceOnly nm _)   = nm
ml_modname (ObjectCode nm _ _) = nm
ml_modname (InPackage  nm _)   = nm

isPackageLoc (InPackage _ _) = True
isPackageLoc _               = False

mkFinder :: [(ModuleName,PackageName,FilePath)] -> [FilePath] -> Finder
mkFinder pkg_ifaces home_dirs modnm
   = do found <- mkFinderX pkg_ifaces home_dirs modnm
        putStrLn ("FINDER: request  = " ++ modnm ++ "\n" ++
                  "FINDER: response = " ++ showSDoc (ppr found))
        return found


mkFinderX :: [(ModuleName,PackageName,FilePath)] -> [FilePath] -> Finder
mkFinderX pkg_ifaces home_dirs modnm
   -- If the module exists both as package and home, emit a warning
   -- and (arbitrarily) choose the user's one.
   = do home_maybe_found <- mapM (homeModuleExists modnm) home_dirs
                         :: IO [Maybe (ModLocation, ClockTime)]
        case (in_package, catMaybes home_maybe_found) of
           ([], []) 
              -> return NotFound
           ([], locs_n_times@(_:_))
              -> return (homeMod locs_n_times)
           ((pkgname,path):_, [])
              -> return (InPackage modnm pkgname)
           (packages, locs_n_times)
              -> do hPutStr stderr ( "GHCI: warning: module `" ++ modnm ++
                                     "' appears as both a home and package module\n")
                    return (homeMod locs_n_times)
     where
        in_package 
           = [(pkgname,path) | (modname,pkgname,path) <- pkg_ifaces, 
                               modname == modnm]
        homeMod :: [(ModLocation, ClockTime)] -> ModLocation
        homeMod locs_n_times
           = fst (maximumBy (\lt1 lt2 -> if snd lt1 > snd lt2 then lt1 else lt2)
                            locs_n_times)
        

-- See if a .hs or (.hi, .o) pair exist on the given path,
-- and return a ModLocation for whichever is younger
homeModuleExists :: ModuleName -> FilePath -> IO (Maybe (ModLocation, ClockTime))
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
        object thi to = Just (ObjectCode modname nm_o nm_hi, max thi to)
        source ths    = Just (SourceOnly modname nm_hs, ths)
        nm = path ++ "/" ++ modname
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
          -> PCI -> IO Finder
newFinder path pci
   = return (mkFinder (module_table pci) [path])

\end{code}
