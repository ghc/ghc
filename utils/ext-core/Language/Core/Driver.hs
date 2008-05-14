{-# OPTIONS -Wall #-}

{- A simple driver that loads, typechecks, prepares, re-typechecks, and interprets the 
    GHC standard Prelude modules and an application module called Main. 

   Note that, if compiled under GHC, this requires a very large heap to run!
-}

import Control.Exception
import Data.List
import Data.Maybe
import Monad
import Prelude hiding (catch)
import System.Cmd
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath

import Language.Core.Core
import Language.Core.Dependencies
import Language.Core.Overrides
import Language.Core.Prims
import Language.Core.Check
import Language.Core.Prep
import Language.Core.Interp
import Language.Core.ParsecParser

-- You may need to change this.
baseDir :: FilePath
baseDir = "../../libraries/"
-- change to True to typecheck library files as well as reading type signatures
typecheckLibs :: Bool
typecheckLibs = False

-- You shouldn't *need* to change anything below this line...                  

-- Code to check that the external and GHC printers print the same results
validateResults :: FilePath -> Module -> IO ()
validateResults origFile m = do
  let genFile = origFile </> "parsed"
  writeFile genFile (show m) 
  resultCode <- system $ "diff -u " ++ origFile ++ " " ++ genFile
  putStrLn $ case resultCode of
    ExitSuccess   -> "Parse validated for " ++ origFile
    ExitFailure 1 -> "Parse failed to validate for " ++ origFile
    _             -> "Error diffing files: " ++ origFile ++ " " ++ genFile
------------------------------------------------------------------------------

data Flag = Test | NoDeps
  deriving Eq

options :: [OptDescr Flag]
options =
  [Option ['t'] ["test"] (NoArg Test) "validate prettyprinted code",
   Option ['n'] ["no-deps"] (NoArg NoDeps) "don't compute dependencies automatically"
  ]

process :: Bool -> (Check.Menv,[Module]) -> (FilePath, Module)
             -> IO (Check.Menv,[Module])
process _ (senv,modules) p@(f,m) | isLib p && not typecheckLibs = do
  -- if it's a library and we set typecheckLibs to False:
  -- prep, but don't typecheck
  m' <- prepM senv m f
  return (senv, modules ++ [m'])
    where isLib (fp,_) = baseDir `isPrefixOf` fp
process doTest (senv,modules) (f, m@(Module mn _ _)) = catch (do
        when doTest $ validateResults f m
	(case checkModule senv m of
           OkC senv' ->
	     do putStrLn $ "Check succeeded for " ++ show mn
                m' <- prepM senv' m f
		case checkModule senv' m' of
                  OkC senv'' ->
		      do putStrLn "Recheck succeeded"
                         return (senv'',modules ++ [m'])
		  FailC s -> 
		      do putStrLn ("Recheck failed: " ++ s)
			 error "quit"
	   FailC s -> error ("Typechecking failed: " ++ s))) handler
   where handler e = do
           putStrLn ("WARNING: we caught an exception " ++ show e 
                     ++ " while processing " ++ f)
           return (senv, modules)

prepM :: Check.Menv -> Module -> FilePath -> IO Module
prepM senv' m _f = do
  let m' = prepModule senv' m
  --writeFile (f </> ".prepped") (show m')
  return m'

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (opts, fnames@(_:_), _) ->
       let doTest      = Test `elem` opts
           computeDeps = NoDeps `notElem` opts in
         doOneProgram computeDeps doTest fnames
    _ -> error "usage: ./Driver [filename]"
  where  doOneProgram :: Bool -> Bool -> [FilePath] -> IO ()
         doOneProgram computeDeps doTest fns = do
               putStrLn $ "========== Program " ++ (show fns) ++ " ============="
               deps <- if computeDeps 
                         then
                           getDependencies fns
                         else (liftM catMaybes) (mapM findModuleDirect fns)
               putStrLn $ "deps = " ++ show (fst (unzip deps))
               {-
                 Note that we scan over the libraries twice:
                 first to gather together all type sigs, then to typecheck them
                 (the latter of which doesn't necessarily have to be done every time.)
                 This is a hack to avoid dealing with circular dependencies. 
                -}
               -- notice: scan over libraries *and* input modules first, not just libs
               topEnv <- mkInitialEnv (snd (unzip deps))
               (_,modules) <- foldM (process doTest) (topEnv,[]) deps
               let succeeded = length modules
               putStrLn ("Finished typechecking. Successfully checked " 
                          ++ show succeeded)
               overridden <- override modules
               result <- evalProgram overridden
               putStrLn ("Result = " ++ show result)
               putStrLn "All done\n============================================="

         mkInitialEnv :: [Module] -> IO Menv
         mkInitialEnv libs = foldM mkTypeEnv initialEnv libs

         mkTypeEnv :: Menv -> Module -> IO Menv
         mkTypeEnv globalEnv m@(Module mn _ _) = 
                catch (return (envsModule globalEnv m)) handler
                  where handler e = do
                          putStrLn ("WARNING: mkTypeEnv caught an exception " ++ show e 
                                    ++ " while processing " ++ show mn)
                          return globalEnv

findModuleDirect :: FilePath -> IO (Maybe (FilePath, Module))
-- kludge to let us run "make libtest" -- 
-- this module (in the Cabal package) causes an uncaught exception
-- from Prelude.chr, which I haven't been able to track down
findModuleDirect fn | "PackageDescription.hcr" `isSuffixOf` fn = return Nothing
findModuleDirect fn = do
  putStrLn $ "Finding " ++ show fn
  res <- parseCore fn
  case res of
    Left err -> error (show err)
    Right m -> return $ Just (fn,m)