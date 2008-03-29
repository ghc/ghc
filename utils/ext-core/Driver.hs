{- A simple driver that loads, typechecks, prepares, re-typechecks, and interprets the 
    GHC standard Prelude modules and an application module called Main. 

   Note that, if compiled under GHC, this requires a very large heap to run!
-}

import Monad
import System.Environment
import System.Cmd
import System.Exit

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
--------                   

-- Code for checking that the external and GHC printers print the same results
testFlag = "-t"

validateResults :: FilePath -> FilePath -> IO ()
validateResults origFile genFile = do
  resultCode <- system $ "diff -u " ++ origFile ++ " " ++ genFile
  putStrLn $ case resultCode of
    ExitSuccess   -> "Parse validated for " ++ origFile
    ExitFailure 1 -> "Parse failed to validate for " ++ origFile
    _             -> "Error diffing files: " ++ origFile ++ " " ++ genFile
------------------------------------------------------------------------------

process :: Bool -> (Check.Menv,[Module]) -> String -> IO (Check.Menv,[Module])
process doTest (senv,modules) f = 
       do putStrLn ("Processing " ++ f)
          resultOrErr <- parseCore f
	  case resultOrErr of
	    Right m -> do 
                        putStrLn "Parse succeeded"
                        let outF = f ++ ".parsed"
			writeFile outF (show m) 
                        when doTest $ (validateResults f outF)
			case checkModule senv m of
			  OkC senv' -> 
			    do putStrLn "Check succeeded"
			       let m' = prepModule senv' m
                               {- writeFile (f ++ ".prepped") (show m') -}
			       case checkModule senv m' of
                                 OkC senv'' ->
				   do putStrLn "Recheck succeeded"
                                      return (senv'',modules ++ [m'])
				 FailC s -> 
				   do putStrLn ("Recheck failed: " ++ s)
				      error "quit"
			  FailC s -> 
			    do putStrLn ("Check failed: " ++ s)
			       error "quit"
            Left err -> do putStrLn ("Parse failed: " ++ show err)
                           error "quit"

main = do args <- getArgs
          let (doTest, fname) = 
                 case args of
                   (f:fn:_) | f == testFlag -> (True,fn)
                   (fn:_)                   -> (False,fn)
                   _                        -> error $ 
                                              "usage: ./Driver [filename]"
          mapM_ (process doTest (initialEnv,[])) (libs ++ [fname])
          (_,modules) <- foldM (process doTest) (initialEnv,[]) (libs ++ [fname])
	  let result = evalProgram modules
	  putStrLn ("Result = " ++ show result)
	  putStrLn "All done"
            where  libs = map (baseDir ++) ["./ghc-prim/GHC/Generics.hcr",
                           "./ghc-prim/GHC/PrimopWrappers.hcr",
                           "./ghc-prim/GHC/Bool.hcr",
                           "./ghc-prim/GHC/IntWord64.hcr",
                           "./base/GHC/Base.hcr",
                           "./base/GHC/List.hcr",
                           "./base/GHC/Enum.hcr",
                           "./base/GHC/Show.hcr",
                           "./base/GHC/Num.hcr",
                           "./base/GHC/ST.hcr",
                           "./base/GHC/Real.hcr",
                           "./base/GHC/STRef.hcr",
                           "./base/GHC/Arr.hcr",
                           "./base/GHC/Float.hcr",
                           "./base/GHC/Read.hcr",
                           "./base/GHC/Ptr.hcr",
                           "./base/GHC/Word.hcr",
                           "./base/GHC/Int.hcr",
                           "./base/GHC/Unicode.hcr",
                           "./base/GHC/IOBase.hcr",
                           "./base/GHC/Err.hcr",
                           "./base/GHC/Exception.hcr",
                           "./base/GHC/Stable.hcr",
                           "./base/GHC/Storable.hcr",
                           "./base/GHC/Pack.hcr",
                           "./base/GHC/Weak.hcr",
                           "./base/GHC/Handle.hcr",
                           "./base/GHC/IO.hcr",
                           "./base/GHC/Dotnet.hcr",
                           "./base/GHC/Environment.hcr",
                           "./base/GHC/Exts.hcr",
                           "./base/GHC/PArr.hcr",
                           "./base/GHC/TopHandler.hcr",
                           "./base/GHC/Desugar.hcr",
                           "./base/Data/Ord.hcr",
                           "./base/Data/Maybe.hcr",
                           "./base/Data/Bits.hcr",
                           "./base/Data/STRef/Lazy.hcr",
                           "./base/Data/Generics/Basics.hcr",
                           "./base/Data/Generics/Aliases.hcr",
                           "./base/Data/Generics/Twins.hcr",
                           "./base/Data/Generics/Instances.hcr",
                           "./base/Data/Generics/Text.hcr",
                           "./base/Data/Generics/Schemes.hcr",
                           "./base/Data/Tuple.hcr",
                           "./base/Data/String.hcr",
                           "./base/Data/Either.hcr",
                           "./base/Data/Char.hcr",
                           "./base/Data/List.hcr",
                           "./base/Data/HashTable.hcr",
                           "./base/Data/Typeable.hcr",
                           "./base/Data/Dynamic.hcr",
                           "./base/Data/Function.hcr",
                           "./base/Data/IORef.hcr",
                           "./base/Data/Fixed.hcr",
                           "./base/Data/Monoid.hcr",
                           "./base/Data/Ratio.hcr",
                           "./base/Data/STRef.hcr",
                           "./base/Data/Version.hcr",
                           "./base/Data/Complex.hcr",
                           "./base/Data/Unique.hcr",
                           "./base/Data/Foldable.hcr",
                           "./base/Data/Traversable.hcr"]