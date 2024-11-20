{-# LANGUAGE ViewPatterns #-}
module Rules.ToolArgs(toolArgsTarget) where

import Rules.Generate
import Development.Shake
import Target
import Context
import Stage
import Expression

import Packages
import Settings
import Hadrian.Oracles.Cabal
import Hadrian.Haskell.Cabal.Type
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import qualified Data.Set as Set
import Oracles.ModuleFiles
import Utilities

-- | @tool:@ is used by tooling in order to get the arguments necessary
-- to set up a GHC API session which can compile modules from GHC. When
-- run, the target prints out the arguments that would be passed to @ghc@
-- during normal compilation to @stdout@ for the file passed as an
-- argument.
--
-- This target is called by the `ghci.sh` script in order to load all of GHC's
-- modules into GHCi. It is invoked with argument `tool:ghc/Main.hs` in
-- that script so that we can load the whole library and executable
-- components into GHCi.
--
-- The suitable arguments for a multi-component session can be queried using
-- `multi:<pkg>`, for example `multi:ghc` will create a session which compiles the
-- `ghc` package and all it's dependencies in a single session. This is what the ./hadrian-multi
-- script uses to set-up a multi session.
--
-- The `multi` target can be used to create a session which loads **everything** that
-- can be built by stage0 compiler, this is probably more than what you need so best stick
-- with `multi:ghc` unless you're a pro.
--


-- | A phony target of form `tool:path/to/file.hs` which returns the
-- options needed to compile the specific file.
toolArgsTarget :: Rules ()
toolArgsTarget = do
  phonys (\s -> if "tool:" `isPrefixOf` s then Just (toolRuleBody (drop 5 s)) else Nothing)

  phonys (\s -> if "multi:" `isPrefixOf` s then Just (multiSetup (Just (drop 6 s))) else Nothing)

  "multi" ~> multiSetup Nothing

multiSetup :: Maybe String -> Action ()
multiSetup pkg_s = do
  -- Find the targets we want to build.
  tool_targets <- case pkg_s of
        Nothing -> return toolTargets
        Just pkg_s -> case findPackageByName pkg_s of
                        Just pkg -> (pkg :) . Set.toList <$> pkg_deps pkg
                        Nothing  -> error $ "Unknown package: " ++ pkg_s
  -- Get the arguments for all the targets
  pargs <- mapM one_args tool_targets
  -- Build any other dependencies (such as generated files)
  liftIO $ writeOutput (concatMap (\x -> ["-unit", x]) (map ( "@" <>) pargs))

  where
    resp_file root p = root </> "multi" </> pkgName p

    pkg_deps pkg = do
      deps <- readPackageData pkg
      let immediate_deps = filter (`elem` toolTargets) (packageDependencies deps)
      trans_deps <- Set.unions <$> mapM pkg_deps immediate_deps
      return (Set.fromList immediate_deps `Set.union` trans_deps)

    one_args p =  do
      putProgressInfo ("Computing arguments for " ++ pkgName p)
      root <- buildRoot
      let fake_target = target (Context stage0InTree p (if windowsHost then vanilla else dynamic) Inplace)
                          (Ghc ToolArgs stage0InTree) [] ["ignored"]
      arg_list <- interpret fake_target getArgs
      let c = Context stage0InTree p (if windowsHost then vanilla else dynamic) Inplace -- Critical use of Inplace, one of the main motivations!
      cd <- readContextData c
      srcs <- hsSources c
      gens <- interpretInContext c generatedDependencies
      need (srcs ++ gens)
      let rexp m = ["-reexported-module", m]
      let hidir = root </> "interfaces" </> pkgPath p
      writeFile' (resp_file root p) (intercalate "\n" (th_hack arg_list
                                                      ++ modules cd
                                                      ++ concatMap rexp (reexportModules cd)
                                                      ++ ["-outputdir", hidir,
                                                          "-this-package-name", pkgName p]))
      return (resp_file root p)


    -- The template-haskell package is compiled with -this-unit-id=template-haskell but
    -- everything which depends on it depends on `-package-id-template-haskell-2.17.0.0`
    -- and so the logic for detetecting which home-units depend on what is defeated.
    -- The workaround here is just to rewrite all the `-package-id` arguments to
    -- point to `template-haskell` instead which works for the multi-repl case.
    -- See #20887
    th_hack :: [String] -> [String]
    th_hack ((isPrefixOf "-package-id template-haskell" -> True) : xs) = "-package-id" : "template-haskell" : xs
    th_hack (x:xs) = x : th_hack xs
    th_hack [] = []


toolRuleBody :: FilePath -> Action ()
toolRuleBody fp = do
  mm <- dirMap
  cfp <- liftIO $ canonicalizePath fp
  case find (flip isPrefixOf cfp . fst) mm  of
    Just (_, (p, extra)) -> mkToolTarget extra p
    Nothing -> fail $ "No prefixes matched " ++ show fp ++ " IN\n " ++ show mm

writeOutput :: [String] -> IO ()
writeOutput args = do
    liftIO $ lookupEnv "TOOL_OUTPUT" >>= \case
      Nothing -> putStrLn (intercalate "\n" args)
      Just out -> writeFile out (intercalate "\n" args)

mkToolTarget :: [String] -> Package -> Action ()
mkToolTarget es p = do
    -- This builds automatically generated dependencies. Not sure how to do
    -- this generically yet.
    putProgressInfo ("Computing arguments for " ++ pkgName p)

    let context = Context stage0InTree p (if windowsHost then vanilla else dynamic) Final
    let fake_target = target context
                        (Ghc ToolArgs stage0InTree) [] ["ignored"]
    -- Generate any source files for this target
    srcs <- hsSources context
    gens <- interpretInContext context generatedDependencies

    -- Build any necessary dependencies
    depPkgIds <- cabalDependencies context
    dep_confs <- mapM (\pkgId -> packageDbPath (PackageDbLoc stage0InTree Final) <&> (-/- pkgId <.> "conf")) depPkgIds

    need (gens ++ srcs ++ dep_confs)

    arg_list <- interpret fake_target getArgs
    liftIO $ writeOutput (arg_list ++ es)

-- This list is quite a lot like stage0packages but doesn't include
-- critically the `exe:ghc` component as that depends on the GHC library
-- which takes a while to compile.
toolTargets :: [Package]
toolTargets = [ binary
              , bytestring
              , cabalSyntax
              , cabal
              , compiler
              , containers
              , directory
              , process
              , exceptions
              , filepath
              -- , ghc     -- # depends on ghc library
              -- , runGhc  -- # depends on ghc library
              , ghcBoot
              , ghcBootTh
              , ghcHeap
              , ghci
              , ghcPkg  -- # executable
              -- , haddock -- # depends on ghc library
              , hsc2hs  -- # executable
              , hpc
              , hpcBin  -- # executable
              , mtl
              , parsec
              , time
              , templateHaskell
              , text
              , transformers
              , semaphoreCompat
              , unlit  -- # executable
              ] ++ if windowsHost then [ win32 ] else [ unix ]

-- | Create a mapping from files to which component it belongs to.
dirMap :: Action [(FilePath, (Package, [String]))]
dirMap = do
  auto <- concatMapM go toolTargets
  -- Mush the ghc executable into the compiler component so the whole of ghc is not built when
  -- configuring
  ghc_exe <- mkGhc
  return (auto ++ [ghc_exe])

  where
    -- Make a separate target for the exe:ghc target because otherwise
    -- configuring would build the whole GHC library which we probably
    -- don't want to do.
    mkGhc = do
      let c = (Context stage0InTree compiler (if windowsHost then vanilla else dynamic) Final)
      cd <- readContextData c
      fp <- liftIO $ canonicalizePath "ghc/"
      return (fp, (compiler, "-ighc" : modules cd ++ otherModules cd ++ ["ghc/Main.hs"]))
    go p = do
      let c = (Context stage0InTree p (if windowsHost then vanilla else dynamic) Final)
      -- readContextData has the effect of configuring the package so all
      -- dependent packages will also be built.
      cd <- readContextData c
      ids <- liftIO $ mapM canonicalizePath [pkgPath p </> i | i <- srcDirs cd]
      return $ map (,(p, modules cd ++ otherModules cd)) ids

