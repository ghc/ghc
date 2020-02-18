{-# LANGUAGE ViewPatterns #-}

import GHC
import GHC.Driver.Make
import GHC.Driver.Session
import GHC.Driver.Finder

import Control.Monad.IO.Class (liftIO)
import Data.List (sort, stripPrefix)
import Data.Either

import System.Environment
import System.Directory
import System.IO

main :: IO ()
main = do
  libdir:args <- getArgs

  runGhc (Just libdir) $
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do

    dflags0 <- getSessionDynFlags
    (dflags1, _, _) <- parseDynamicFlags dflags0 $ map noLoc $
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

    tgt <- guessTarget "A" Nothing
    setTargets [tgt]
    hsc_env <- getSession

    liftIO $ do

    _emss <- downsweep hsc_env [] [] False

    flushFinderCaches hsc_env
    createDirectoryIfMissing False "mydir"
    renameFile "B.hs" "mydir/B.hs"

    emss <- downsweep hsc_env [] [] False

    -- If 'checkSummaryTimestamp' were to call 'addHomeModuleToFinder' with
    -- (ms_location old_summary) like summariseFile used to instead of
    -- using the 'location' parameter we'd end up using the old location of
    -- the "B" module in this test. Make sure that doesn't happen.

    hPrint stderr $ sort (map (ml_hs_file . ms_location) (rights emss))

writeMod :: [String] -> IO ()
writeMod src@(head -> stripPrefix "module " -> Just (takeWhile (/=' ') -> mod))
  = writeFile (mod++".hs") $ unlines src
