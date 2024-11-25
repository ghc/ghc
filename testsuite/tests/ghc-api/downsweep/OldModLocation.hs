{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ViewPatterns #-}

import GHC
import GHC.Driver.Make
import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Types.Error (mkUnknownDiagnostic)
import GHC.Unit.Module.Graph
import GHC.Unit.Finder

import Control.Monad.IO.Class (liftIO)
import Data.List (sort, stripPrefix)
import Data.Either
import Data.Maybe

import System.Environment
import System.Directory
import System.IO

main :: IO ()
main = do
  libdir:args <- getArgs

  runGhc (Just libdir) $
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do

    dflags0 <- getSessionDynFlags
    logger <- getLogger
    (dflags1, _, _) <- parseDynamicFlags logger dflags0 $ map noLoc $
        [ "-i", "-i.", "-imydir"
        -- , "-v3"
        ] ++ args
    _ <- setSessionDynFlags dflags1

    liftIO $ mapM_ writeMod
      [ [ "module A where"
        , "import B"
        ]
      , [ "module B where"
        ]
      ]

    tgt <- guessTarget "A" Nothing Nothing
    setTargets [tgt]
    hsc_env <- getSession

    liftIO $ do

    _emss <- downsweep hsc_env mkUnknownDiagnostic Nothing [] [] False

    flushFinderCaches (hsc_FC hsc_env) (hsc_unit_env hsc_env)
    createDirectoryIfMissing False "mydir"
    renameFile "B.hs" "mydir/B.hs"

    (_, nodes) <- downsweep hsc_env mkUnknownDiagnostic Nothing [] [] False

    -- If 'checkSummaryTimestamp' were to call 'addHomeModuleToFinder' with
    -- (ms_location old_summary) like summariseFile used to instead of
    -- using the 'location' parameter we'd end up using the old location of
    -- the "B" module in this test. Make sure that doesn't happen.

    hPrint stderr $ sort (map (ml_hs_file . ms_location) (mgModSummaries nodes))

writeMod :: [String] -> IO ()
writeMod src@(head -> stripPrefix "module " -> Just (takeWhile (/=' ') -> mod))
  = writeFile (mod++".hs") $ unlines src
