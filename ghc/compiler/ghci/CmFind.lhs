%
% (c) The University of Glasgow, 2000
%
\section[CmFind]{Module finder for GHCI}

\begin{code}
module CmFind ( Path, ModName, PkgName,
                ModLocation(..), ml_modname, isPackageLoc,
		Finder, newFinder )
where

#include "HsVersions.h"

import IO		( hPutStr, stderr )
import List		( maximumBy )
import Maybe		( catMaybes )
import Char		( isUpper )
import List		( nub )
import Time		( ClockTime )
import Directory	( doesFileExist, getModificationTime,
                          getDirectoryContents) 

import Module		( Module )
import CmStaticInfo	( PCI, Package(..) )
\end{code}

\begin{code}
type Path    = String
type ModName = String
type PkgName = String

data ModLocation 
   = SourceOnly ModName Path        -- .hs
   | ObjectCode ModName Path Path   -- .o, .hi
   | InPackage  ModName PkgName
   | NotFound
     deriving Show

type Finder = ModName -> IO ModLocation

ml_modname (SourceOnly nm _)   = nm
ml_modname (ObjectCode nm _ _) = nm
ml_modname (InPackage  nm _)   = nm

isPackageLoc (InPackage _ _) = True
isPackageLoc _               = False

mkFinder :: [(ModName,PkgName,Path)] -> [Path] -> Finder
mkFinder pkg_ifaces home_dirs modnm
   = do found <- mkFinderX pkg_ifaces home_dirs modnm
        putStrLn ("FINDER: request  = " ++ modnm ++ "\n" ++
                  "FINDER: response = " ++ show found)
        return found


mkFinderX :: [(ModName,PkgName,Path)] -> [Path] -> Finder
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
homeModuleExists :: ModName -> Path -> IO (Maybe (ModLocation, ClockTime))
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



newFinder :: PCI -> IO Finder
newFinder pci
   -- PCI is a list of packages and their names
   = do 
        -- the list of directories where package interfaces are
        let p_i_dirs :: [(PkgName,Path)]
            p_i_dirs = concatMap nm_and_paths pci

        -- interface names in each directory
        ifacess <- mapM ifaces_in_dir p_i_dirs
        let ifaces :: [(ModName,PkgName,Path)] 
            ifaces = concat ifacess

        -- ToDo: allow a range of home package directories
        return (mkFinder ifaces ["."])
     where
        nm_and_paths :: Package -> [(PkgName,Path)]
        nm_and_paths package 
           = [(name package, path) | path <- nub (import_dirs package)]

        ifaces_in_dir :: (PkgName,Path) -> IO [(ModName,PkgName,Path)]
        ifaces_in_dir (pkgname,path)
           = getDirectoryContents path >>= \ entries ->
             return [(zap_hi if_nm, pkgname, path) 
                    | if_nm <- entries, looks_like_iface_name if_nm]
        looks_like_iface_name e
           = not (null e) && isUpper (head e) 
                          && take 3 (reverse e) == "ih."
        zap_hi 
           = reverse . drop 3 . reverse

\end{code}
