{- A simple driver that loads, typechecks, prepares, re-typechecks, and interprets the 
    GHC standard Prelude modules and an application module called Main. 

   Note that, if compiled under GHC, this requires a very large heap to run!
-}

import Control.Exception
import Data.List
import Maybe
import Monad
import Prelude hiding (catch)
import System.Cmd
import System.Environment
import System.Exit
import System.FilePath

import Core
import Printer
import ParsecParser
import Env
import Prims
import Check
import Prep
import Interp

-- You may need to change this.
baseDir = "../../libraries/"
-- change to True to typecheck library files as well as reading type signatures
typecheckLibs = False 

-- You shouldn't *need* to change anything below this line...                  
libDir = map (baseDir ++)

-- Code to check that the external and GHC printers print the same results
testFlag = "-t"

validateResults :: FilePath -> FilePath -> IO ()
validateResults origFile genFile = do
  resultCode <- system $ "diff -u " ++ origFile ++ " " ++ genFile
  putStrLn $ case resultCode of
    ExitSuccess   -> "Parse validated for " ++ origFile
    ExitFailure 1 -> "Parse failed to validate for " ++ origFile
    _             -> "Error diffing files: " ++ origFile ++ " " ++ genFile
------------------------------------------------------------------------------

process :: Bool -> (Check.Menv,[Module]) -> FilePath 
             -> IO (Check.Menv,[Module])
process doTest (senv,modules) f = catch
      (do putStrLn ("Processing " ++ f)
          resultOrErr <- parseCore f
	  case resultOrErr of
	    Right m@(Module mn _ _) -> do 
                        putStrLn "Parse succeeded"
                        let outF = f ++ ".parsed"
			writeFile outF (show m) 
                        when doTest $ (validateResults f outF)
			case checkModule senv m of
			  OkC senv' -> 
			    do putStrLn $ "Check succeeded for " ++ show mn 
			       let m' = prepModule senv' m
                               let (dir,fname) = splitFileName f
                               let preppedFile = dir </> (fname ++ ".prepped") 
                               writeFile preppedFile (show m') 
			       case checkModule senv' m' of
                                 OkC senv'' ->
				   do putStrLn "Recheck succeeded"
                                      return (senv'',modules ++ [m'])
				 FailC s -> 
				   do putStrLn ("Recheck failed: " ++ s)
				      error "quit"
			  FailC s -> error ("Typechecking failed: " ++ s)
            Left err -> error ("Parsing failed: " ++ show err)) handler
   where handler e = do
           putStrLn ("WARNING: we caught an exception " ++ show e 
                     ++ " while processing " ++ f)
           return (senv, modules)

main = do args <- getArgs
          let (doTest, fnames) = 
                 case args of
                   (f:rest) | f == testFlag -> (True,rest)
                   rest@(_:_)                   -> (False,rest)
                   _                        -> error $ 
                                              "usage: ./Driver [filename]"
          -- Note that we scan over the libraries twice:
          -- first to gather together all type sigs, then to typecheck them
          -- (the latter of which doesn't necessarily have to be done every time.)
          -- This is a hack to avoid dealing with circular dependencies.

          -- notice: scan over libraries *and* input modules first, not just libs
          topEnv <- mkInitialEnv (map normalise libs `union` map normalise fnames)
          doOneProgram doTest topEnv fnames
            where  doOneProgram doTest topEnv fns = do
                      putStrLn $ "========== Program " ++ (show fns) ++ " ================"
                      let numToDo = length (typecheckLibraries fns)
                      (_,modules) <- foldM (process doTest) (topEnv,[]) (typecheckLibraries fns)
                      let succeeded = length modules
                      putStrLn ("Finished typechecking. Successfully checked " ++ show succeeded
                         ++ " out of " ++ show numToDo ++ " modules.")
                      -- TODO: uncomment once interpreter works
                      --let result = evalProgram modules
                      --putStrLn ("Result = " ++ show result)
                      putStrLn "All done\n============================================="

                   typecheckLibraries = if typecheckLibs then (libs ++) else id
                   -- Just my guess as to what's needed from the base libs.
                   -- May well be missing some libraries and have some that
                   -- aren't commonly used.
                   -- However, the following is enough to check all of nofib.
                   -- This points to how nice it would be to have explicit import lists in ext-core.
                   libs = (libDir ["./ghc-prim/GHC/Generics.hcr",
                           "./ghc-prim/GHC/Bool.hcr",
                           "./ghc-prim/GHC/IntWord64.hcr",
                           "./base/GHC/Base.hcr",
                           "./base/Data/Tuple.hcr",
                           "./base/Data/Maybe.hcr",
                           "./integer-gmp/GHC/Integer.hcr",
                           "./base/GHC/List.hcr",
                           "./base/GHC/Enum.hcr",
                           "./base/Data/Ord.hcr",
                           "./base/Data/String.hcr",
                           "./base/Data/Either.hcr",
                           "./base/GHC/Show.hcr",
                           "./base/GHC/Num.hcr",
                           "./base/GHC/ST.hcr",
                           "./base/GHC/STRef.hcr",
                           "./base/GHC/Arr.hcr",
                           "./base/GHC/Real.hcr",
                           "./base/Control/Monad.hcr",
                           "./base/GHC/Int.hcr",
                           "./base/GHC/Unicode.hcr",
                           "./base/Text/ParserCombinators/ReadP.hcr",
                           "./base/Text/Read/Lex.hcr",
                           "./base/Text/ParserCombinators/ReadPrec.hcr",
                           "./base/GHC/Read.hcr",
                           "./base/GHC/Word.hcr",
                           "./base/Data/HashTable.hcr",
                           "./base/Unsafe/Coerce.hcr",
                           "./base/Foreign/Storable.hcr",
                           "./base/Foreign/C/Types.hcr",
                           "./base/GHC/IOBase.hcr",                           
                           "./base/GHC/ForeignPtr.hcr",
                           "./base/Data/Typeable.hcr",
                           "./base/Data/Dynamic.hcr",
                           "./base/GHC/Err.hcr",
                           "./base/Data/List.hcr",
                           "./base/Data/Char.hcr",
                           "./base/GHC/Pack.hcr",
                           "./base/GHC/Storable.hcr",
                           "./base/System/IO/Error.hcr",
                           "./base/Foreign/Ptr.hcr",
                           "./base/Foreign/Marshal/Error.hcr",
                           "./base/Foreign/ForeignPtr.hcr",
                           "./base/Foreign/Marshal/Alloc.hcr",
                           "./base/Foreign/Marshal/Utils.hcr",
                           "./base/Foreign/Marshal/Array.hcr",
                           "./base/Foreign/C/String.hcr",
                           "./base/Foreign/C/Error.hcr",
                           "./base/Foreign/C.hcr",
                           "./base/System/IO/Unsafe.hcr",
                           "./base/Foreign/Marshal.hcr",
                           "./base/Foreign/StablePtr.hcr",
                           "./base/Foreign.hcr",
                           "./base/System/Posix/Types.hcr",
                           "./base/System/Posix/Internals.hcr",
                           "./base/GHC/Conc.hcr",
                           "./base/Control/Exception.hcr",
                           "./base/GHC/TopHandler.hcr",
                           "./base/Data/Bits.hcr",
                           "./base/Numeric.hcr",
                           "./base/GHC/Ptr.hcr",
                           "./base/GHC/Float.hcr",
                           "./base/GHC/Exception.hcr",
                           "./base/GHC/Stable.hcr",
                           "./base/GHC/Weak.hcr",
                           "./base/GHC/Handle.hcr",
                           "./base/GHC/IO.hcr",
                           "./base/GHC/Dotnet.hcr",
                           "./base/GHC/Environment.hcr",
                           "./base/GHC/Exts.hcr",
                           "./base/GHC/PArr.hcr",
                           "./base/System/IO.hcr",
                           "./base/System/Environment.hcr",
                           "./base/Data/Generics/Basics.hcr",
                           "./base/Data/Complex.hcr",
                           "./array/Data/Array/Base.hcr",
                           "./base/System/Exit.hcr",
                           "./base/Data/Ratio.hcr",
                           "./base/Control/Monad/ST/Lazy.hcr",
                           "./base/Prelude.hcr",
                           "./base/Control/Concurrent/MVar.hcr",
                           "./base/Data/Foldable.hcr"])

mkInitialEnv :: [FilePath] -> IO Menv
mkInitialEnv libs = foldM mkTypeEnv initialEnv libs

mkTypeEnv :: Menv -> FilePath -> IO Menv
mkTypeEnv globalEnv fn = catch (do
  putStrLn $ "mkTypeEnv: reading library " ++ fn
  resultOrErr <- parseCore fn
  case resultOrErr of
    Right mod@(Module mn _ _) -> do
       let newE = envsModule globalEnv mod
       return newE 
    Left  err -> do putStrLn ("Failed to parse library module: " ++ show err)
                    error "quit") handler
  where handler e = do
           putStrLn ("WARNING: mkTypeEnv caught an exception " ++ show e 
                     ++ " while processing " ++ fn)
           return globalEnv
