{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

-- | This test checks if 'downsweep can return partial results when vaious
-- kinds of parse errors occur in modules.

import GHC
import GhcMake
import DynFlags
import Outputable
import Exception (ExceptionMonad, ghandle)

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Data.IORef
import Data.List
import Data.Either

import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe (unsafePerformIO)

any_failed :: IORef Bool
any_failed = unsafePerformIO $ newIORef False
{-# NOINLINE any_failed #-}

it :: ExceptionMonad m => [Char] -> m Bool -> m ()
it msg act =
    ghandle (\(_ex :: AssertionFailed) -> dofail) $
    ghandle (\(_ex :: ExitCode) -> dofail) $ do
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
    (dflags1, _, _) <- parseDynamicFlags dflags0 $ map noLoc $
        [ -- "-v3"
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

  errored <- readIORef any_failed
  when errored $ exitFailure
  return ()


go :: String -> [[String]] -> ([ModSummary] -> Ghc Bool) -> Ghc ()
go label mods cnd =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    liftIO $ hPutStrLn stderr $ "== " ++ label

    liftIO $ mapM_ writeMod mods

    tgt <- guessTarget "A" Nothing

    setTargets [tgt]

    hsc_env <- getSession
    emss <- liftIO $ downsweep hsc_env [] [] False
    -- liftIO $ hPutStrLn stderr $ showSDocUnsafe $ ppr $ rights emss

    it label $ cnd (rights emss)


writeMod :: [String] -> IO ()
writeMod src@(head -> stripPrefix "module " -> Just (takeWhile (/=' ') -> mod))
  = writeFile (mod++".hs") $ unlines src
