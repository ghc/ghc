-----------------------------------------------------------------------------
--
-- Makefile Dependency Generation
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module DriverMkDepend (
        doMkDependHS
  ) where

#include "HsVersions.h"

import qualified GHC
import GhcMonad
import HsSyn            ( ImportDecl(..) )
import DynFlags
import Util
import HscTypes
import SysTools         ( newTempName )
import qualified SysTools
import Module
import Digraph          ( SCC(..) )
import Finder
import Outputable
import Panic
import SrcLoc
import Data.List
import FastString

import Exception
import ErrUtils

import System.Directory
import System.FilePath
import System.IO
import System.IO.Error  ( isEOFError )
import Control.Monad    ( when )
import Data.Maybe       ( isJust )

-----------------------------------------------------------------
--
--              The main function
--
-----------------------------------------------------------------

doMkDependHS :: GhcMonad m => [FilePath] -> m ()
doMkDependHS srcs = do
    -- Initialisation
    dflags0 <- GHC.getSessionDynFlags

    -- We kludge things a bit for dependency generation. Rather than
    -- generating dependencies for each way separately, we generate
    -- them once and then duplicate them for each way's osuf/hisuf.
    -- We therefore do the initial dependency generation with an empty
    -- way and .o/.hi extensions, regardless of any flags that might
    -- be specified.
    let dflags = dflags0 {
                     ways = [],
                     buildTag = mkBuildTag [],
                     hiSuf = "hi",
                     objectSuf = "o"
                 }
    _ <- GHC.setSessionDynFlags dflags

    when (null (depSuffixes dflags)) $ liftIO $
        throwGhcExceptionIO (ProgramError "You must specify at least one -dep-suffix")

    files <- liftIO $ beginMkDependHS dflags

    -- Do the downsweep to find all the modules
    targets <- mapM (\s -> GHC.guessTarget s Nothing) srcs
    GHC.setTargets targets
    let excl_mods = depExcludeMods dflags
    mod_summaries <- GHC.depanal excl_mods True {- Allow dup roots -}

    -- Sort into dependency order
    -- There should be no cycles
    let sorted = GHC.topSortModuleGraph False mod_summaries Nothing

    -- Print out the dependencies if wanted
    liftIO $ debugTraceMsg dflags 2 (text "Module dependencies" $$ ppr sorted)

    -- Prcess them one by one, dumping results into makefile
    -- and complaining about cycles
    hsc_env <- getSession
    root <- liftIO getCurrentDirectory
    mapM_ (liftIO . processDeps dflags hsc_env excl_mods root (mkd_tmp_hdl files)) sorted

    -- If -ddump-mod-cycles, show cycles in the module graph
    liftIO $ dumpModCycles dflags mod_summaries

    -- Tidy up
    liftIO $ endMkDependHS dflags files

    -- Unconditional exiting is a bad idea.  If an error occurs we'll get an
    --exception; if that is not caught it's fine, but at least we have a
    --chance to find out exactly what went wrong.  Uncomment the following
    --line if you disagree.

    --`GHC.ghcCatch` \_ -> io $ exitWith (ExitFailure 1)

-----------------------------------------------------------------
--
--              beginMkDependHs
--      Create a temporary file,
--      find the Makefile,
--      slurp through it, etc
--
-----------------------------------------------------------------

data MkDepFiles
  = MkDep { mkd_make_file :: FilePath,          -- Name of the makefile
            mkd_make_hdl  :: Maybe Handle,      -- Handle for the open makefile
            mkd_tmp_file  :: FilePath,          -- Name of the temporary file
            mkd_tmp_hdl   :: Handle }           -- Handle of the open temporary file

beginMkDependHS :: DynFlags -> IO MkDepFiles
beginMkDependHS dflags = do
        -- open a new temp file in which to stuff the dependency info
        -- as we go along.
  tmp_file <- newTempName dflags "dep"
  tmp_hdl <- openFile tmp_file WriteMode

        -- open the makefile
  let makefile = depMakefile dflags
  exists <- doesFileExist makefile
  mb_make_hdl <-
        if not exists
        then return Nothing
        else do
           makefile_hdl <- openFile makefile ReadMode

                -- slurp through until we get the magic start string,
                -- copying the contents into dep_makefile
           let slurp = do
                l <- hGetLine makefile_hdl
                if (l == depStartMarker)
                        then return ()
                        else do hPutStrLn tmp_hdl l; slurp

                -- slurp through until we get the magic end marker,
                -- throwing away the contents
           let chuck = do
                l <- hGetLine makefile_hdl
                if (l == depEndMarker)
                        then return ()
                        else chuck

           catchIO slurp
                (\e -> if isEOFError e then return () else ioError e)
           catchIO chuck
                (\e -> if isEOFError e then return () else ioError e)

           return (Just makefile_hdl)


        -- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depStartMarker

  return (MkDep { mkd_make_file = makefile, mkd_make_hdl = mb_make_hdl,
                  mkd_tmp_file  = tmp_file, mkd_tmp_hdl  = tmp_hdl})


-----------------------------------------------------------------
--
--              processDeps
--
-----------------------------------------------------------------

processDeps :: DynFlags
            -> HscEnv
            -> [ModuleName]
            -> FilePath
            -> Handle           -- Write dependencies to here
            -> SCC ModSummary
            -> IO ()
-- Write suitable dependencies to handle
-- Always:
--                      this.o : this.hs
--
-- If the dependency is on something other than a .hi file:
--                      this.o this.p_o ... : dep
-- otherwise
--                      this.o ...   : dep.hi
--                      this.p_o ... : dep.p_hi
--                      ...
-- (where .o is $osuf, and the other suffixes come from
-- the cmdline -s options).
--
-- For {-# SOURCE #-} imports the "hi" will be "hi-boot".

processDeps dflags _ _ _ _ (CyclicSCC nodes)
  =     -- There shouldn't be any cycles; report them
    throwGhcExceptionIO (ProgramError (showSDoc dflags $ GHC.cyclicModuleErr nodes))

processDeps dflags hsc_env excl_mods root hdl (AcyclicSCC node)
  = do  { let extra_suffixes = depSuffixes dflags
              include_pkg_deps = depIncludePkgDeps dflags
              src_file  = msHsFilePath node
              obj_file  = msObjFilePath node
              obj_files = insertSuffixes obj_file extra_suffixes

              do_imp loc is_boot pkg_qual imp_mod
                = do { mb_hi <- findDependency hsc_env loc pkg_qual imp_mod
                                               is_boot include_pkg_deps
                     ; case mb_hi of {
                           Nothing      -> return () ;
                           Just hi_file -> do
                     { let hi_files = insertSuffixes hi_file extra_suffixes
                           write_dep (obj,hi) = writeDependency root hdl [obj] hi

                        -- Add one dependency for each suffix;
                        -- e.g.         A.o   : B.hi
                        --              A.x_o : B.x_hi
                     ; mapM_ write_dep (obj_files `zip` hi_files) }}}


                -- Emit std dependency of the object(s) on the source file
                -- Something like       A.o : A.hs
        ; writeDependency root hdl obj_files src_file

                -- Emit a dependency for each import

        ; let do_imps is_boot idecls = sequence_
                    [ do_imp loc is_boot (ideclPkgQual i) mod
                    | L loc i <- idecls,
                      let mod = unLoc (ideclName i),
                      mod `notElem` excl_mods ]

        ; do_imps True  (ms_srcimps node)
        ; do_imps False (ms_imps node)
        }


findDependency  :: HscEnv
                -> SrcSpan
                -> Maybe FastString     -- package qualifier, if any
                -> ModuleName           -- Imported module
                -> IsBootInterface      -- Source import
                -> Bool                 -- Record dependency on package modules
                -> IO (Maybe FilePath)  -- Interface file file
findDependency hsc_env srcloc pkg imp is_boot include_pkg_deps
  = do  {       -- Find the module; this will be fast because
                -- we've done it once during downsweep
          r <- findImportedModule hsc_env imp pkg
        ; case r of
            Found loc _
                -- Home package: just depend on the .hi or hi-boot file
                | isJust (ml_hs_file loc) || include_pkg_deps
                -> return (Just (addBootSuffix_maybe is_boot (ml_hi_file loc)))

                -- Not in this package: we don't need a dependency
                | otherwise
                -> return Nothing

            fail ->
                let dflags = hsc_dflags hsc_env
                in throwOneError $ mkPlainErrMsg dflags srcloc $
                        cannotFindModule dflags imp fail
        }

-----------------------------
writeDependency :: FilePath -> Handle -> [FilePath] -> FilePath -> IO ()
-- (writeDependency r h [t1,t2] dep) writes to handle h the dependency
--      t1 t2 : dep
writeDependency root hdl targets dep
  = do let -- We need to avoid making deps on
           --     c:/foo/...
           -- on cygwin as make gets confused by the :
           -- Making relative deps avoids some instances of this.
           dep' = makeRelative root dep
           forOutput = escapeSpaces . reslash Forwards . normalise
           output = unwords (map forOutput targets) ++ " : " ++ forOutput dep'
       hPutStrLn hdl output

-----------------------------
insertSuffixes
        :: FilePath     -- Original filename;   e.g. "foo.o"
        -> [String]     -- Suffix prefixes      e.g. ["x_", "y_"]
        -> [FilePath]   -- Zapped filenames     e.g. ["foo.x_o", "foo.y_o"]
        -- Note that that the extra bit gets inserted *before* the old suffix
        -- We assume the old suffix contains no dots, so we know where to
        -- split it
insertSuffixes file_name extras
  = [ basename <.> (extra ++ suffix) | extra <- extras ]
  where
    (basename, suffix) = case splitExtension file_name of
                         -- Drop the "." from the extension
                         (b, s) -> (b, drop 1 s)


-----------------------------------------------------------------
--
--              endMkDependHs
--      Complete the makefile, close the tmp file etc
--
-----------------------------------------------------------------

endMkDependHS :: DynFlags -> MkDepFiles -> IO ()

endMkDependHS dflags
   (MkDep { mkd_make_file = makefile, mkd_make_hdl =  makefile_hdl,
            mkd_tmp_file  = tmp_file, mkd_tmp_hdl  =  tmp_hdl })
  = do
  -- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depEndMarker

  case makefile_hdl of
     Nothing  -> return ()
     Just hdl -> do

          -- slurp the rest of the original makefile and copy it into the output
        let slurp = do
                l <- hGetLine hdl
                hPutStrLn tmp_hdl l
                slurp

        catchIO slurp
                (\e -> if isEOFError e then return () else ioError e)

        hClose hdl

  hClose tmp_hdl  -- make sure it's flushed

        -- Create a backup of the original makefile
  when (isJust makefile_hdl)
       (SysTools.copy dflags ("Backing up " ++ makefile)
          makefile (makefile++".bak"))

        -- Copy the new makefile in place
  SysTools.copy dflags "Installing new makefile" tmp_file makefile


-----------------------------------------------------------------
--              Module cycles
-----------------------------------------------------------------

dumpModCycles :: DynFlags -> [ModSummary] -> IO ()
dumpModCycles dflags mod_summaries
  | not (dopt Opt_D_dump_mod_cycles dflags)
  = return ()

  | null cycles
  = putMsg dflags (ptext (sLit "No module cycles"))

  | otherwise
  = putMsg dflags (hang (ptext (sLit "Module cycles found:")) 2 pp_cycles)
  where

    cycles :: [[ModSummary]]
    cycles = [ c | CyclicSCC c <- GHC.topSortModuleGraph True mod_summaries Nothing ]

    pp_cycles = vcat [ (ptext (sLit "---------- Cycle") <+> int n <+> ptext (sLit "----------"))
                        $$ pprCycle c $$ blankLine
                     | (n,c) <- [1..] `zip` cycles ]

pprCycle :: [ModSummary] -> SDoc
-- Print a cycle, but show only the imports within the cycle
pprCycle summaries = pp_group (CyclicSCC summaries)
  where
    cycle_mods :: [ModuleName]  -- The modules in this cycle
    cycle_mods = map (moduleName . ms_mod) summaries

    pp_group (AcyclicSCC ms) = pp_ms ms
    pp_group (CyclicSCC mss)
        = ASSERT( not (null boot_only) )
                -- The boot-only list must be non-empty, else there would
                -- be an infinite chain of non-boot imoprts, and we've
                -- already checked for that in processModDeps
          pp_ms loop_breaker $$ vcat (map pp_group groups)
        where
          (boot_only, others) = partition is_boot_only mss
          is_boot_only ms = not (any in_group (map (ideclName.unLoc) (ms_imps ms)))
          in_group (L _ m) = m `elem` group_mods
          group_mods = map (moduleName . ms_mod) mss

          loop_breaker = head boot_only
          all_others   = tail boot_only ++ others
          groups = GHC.topSortModuleGraph True all_others Nothing

    pp_ms summary = text mod_str <> text (take (20 - length mod_str) (repeat ' '))
                       <+> (pp_imps empty (map (ideclName.unLoc) (ms_imps summary)) $$
                            pp_imps (ptext (sLit "{-# SOURCE #-}")) (map (ideclName.unLoc) (ms_srcimps summary)))
        where
          mod_str = moduleNameString (moduleName (ms_mod summary))

    pp_imps :: SDoc -> [Located ModuleName] -> SDoc
    pp_imps _    [] = empty
    pp_imps what lms
        = case [m | L _ m <- lms, m `elem` cycle_mods] of
            [] -> empty
            ms -> what <+> ptext (sLit "imports") <+>
                                pprWithCommas ppr ms

-----------------------------------------------------------------
--
--              Flags
--
-----------------------------------------------------------------

depStartMarker, depEndMarker :: String
depStartMarker = "# DO NOT DELETE: Beginning of Haskell dependencies"
depEndMarker   = "# DO NOT DELETE: End of Haskell dependencies"

