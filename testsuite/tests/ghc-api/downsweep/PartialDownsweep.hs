{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

-- | This test checks if 'downsweep can return partial results when various
-- kinds of parse errors occur in modules.

import GHC
import GHC.Driver.Make
import GHC.Driver.Session
import GHC.Types.Error (mkUnknownDiagnostic)
import GHC.Utils.Outputable
import GHC.Utils.Exception (ExceptionMonad)
import GHC.Data.Bag
import GHC.Unit.Module.Graph

import Control.Monad
import Control.Monad.Catch as MC (handle)
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Data.IORef
import Data.List (sort, find, stripPrefix, isPrefixOf, isSuffixOf)
import Data.Either
import Data.Maybe

import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe (unsafePerformIO)

any_failed :: IORef Bool
any_failed = unsafePerformIO $ newIORef False
{-# NOINLINE any_failed #-}

it :: ExceptionMonad m => [Char] -> m Bool -> m ()
it msg act =
    MC.handle (\(_ex :: AssertionFailed) -> dofail) $
    MC.handle (\(_ex :: ExitCode) -> dofail) $ do
    res <- act
    case res of
      False -> dofail
      True -> return ()
  where
   dofail = do
     liftIO $ hPutStrLn stderr $ "FAILED: " ++ msg
     liftIO $ writeIORef any_failed True

main :: IO ()
main = do
  libdir:args <- getArgs

  runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    logger <- getLogger
    (dflags1, _, _) <- parseDynamicFlags logger dflags0 $ map noLoc $
        [ "-fno-diagnostics-show-caret"
        -- , "-v3"
        ] ++ args
    _ <- setSessionDynFlags dflags1

    go "Parse error in export list"
        [ [ "module A where"
          , "import B"
          ]
        , [ "module B !parse_error where"
          --  ^ this used to cause getImports to throw an exception instead
          --  of having downsweep return an error for just this module
          , "import C"
          ]
        , [ "module C where"
          ]
        ]
       (\mss -> return $
         sort (map (moduleNameString . moduleName . ms_mod) mss) == ["A"]
       )

    go "Parse error in export list with bypass module"
        [ [ "module A where"
          , "import B"
          , "import C"
          ]
        , [ "module B !parse_error where"
          , "import D"
          ]
        , [ "module C where"
          , "import D"
          ]
        , [ "module D where"
          ]
        ]
       (\mss -> return $
           sort (map (moduleNameString . moduleName . ms_mod) mss) == ["A", "C", "D"]
       )
    go "Parse error in import list"
        [ [ "module A where"
          , "import B"
          ]
        , [ "module B where"
          , "!parse_error"
          --  ^ this is silently ignored, getImports assumes the import
          -- list is just empty. This smells like a parser bug to me but
          -- I'm still documenting this behaviour here.
          , "import C"
          ]
        , [ "module C where"
          ]
        ]
       (\mss -> return $
         sort (map (moduleNameString . moduleName . ms_mod) mss) == ["A", "B"]
       )

    go "CPP preprocessor error"
        [ [ "module A where"
          , "import B"
          ]
        , [ "{-# LANGUAGE CPP #-}"
          , "#elif <- cpp error here"
          , "module B where"
          , "import C"
          ]
        , [ "module C where"
          ]
        ]
       (\mss -> return $
         sort (map (moduleNameString . moduleName . ms_mod) mss) == ["A"]
       )

    go "CPP preprocessor error with bypass"
        [ [ "module A where"
          , "import B"
          , "import C"
          ]
        , [ "{-# LANGUAGE CPP #-}"
          , "#elif <- cpp error here"
          , "module B where"
          , "import C"
          ]
        , [ "module C where"
          ]
        ]
       (\mss -> return $
         sort (map (moduleNameString . moduleName . ms_mod) mss) == ["A", "C"]
       )

    go "Import error"
        [ [ "module A where"
          , "import B"
          , "import DoesNotExist_FooBarBaz"
          ]
        , [ "module B where"
          ]
        ]
       (\mss -> return $
           sort (map (moduleNameString . moduleName . ms_mod) mss) == ["A", "B"]
       )

  errored <- readIORef any_failed
  when errored $ exitFailure
  return ()


go :: String -> [[String]] -> ([ModSummary] -> Ghc Bool) -> Ghc ()
go label mods cnd =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    liftIO $ hPutStrLn stderr $ "== " ++ label

    liftIO $ mapM_ writeMod mods

    tgt <- guessTarget "A" Nothing Nothing

    setTargets [tgt]

    hsc_env <- getSession
    (_, nodes) <- liftIO $ downsweep hsc_env mkUnknownDiagnostic Nothing [] [] False

    it label $ cnd (mgModSummaries nodes)


writeMod :: [String] -> IO ()
writeMod src =
    writeFile (mod++".hs") $ unlines src
  where
    Just modline = find ("module" `isPrefixOf`) src
    Just (takeWhile (/=' ') -> mod) = stripPrefix "module " modline
