{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Driver.Session
import GHC

import Control.Monad
import Control.Monad.Catch as MC (try)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Utils.Exception
import GHC.Parser.Header
import GHC.Types.Target
import GHC.Types.SourceError
import GHC.Utils.Outputable
import GHC.Driver.Env
import GHC.Data.StringBuffer
import System.Directory
import System.Environment
import System.Process
import System.IO
import Text.Printf

main :: IO ()
main = do
  libdir:args <- getArgs
  createDirectoryIfMissing False "outdir"
  runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    logger <- getLogger
    (dflags1, xs, warn) <- parseDynamicFlags logger dflags0 $ map noLoc $
        [ "-outputdir", "./outdir"
        , "-fno-diagnostics-show-caret"
        , "-fprint-error-index-links=never"
        ] ++ args
    _ <- setSessionDynFlags dflags1

    -- This test fails on purpose to check if the error message mentions
    -- the source file and not the intermediary preprocessor input file
    -- even when no preprocessor is in use. Just a sanity check.
    go "Error" ["A"]
    --  ^        ^-- targets
    --  ^-- test name
      [("A"           -- this module's name
       , ""           -- pragmas
       , []           -- imports/non exported decls
       , [("x", "z")] -- exported decls
       , OnDisk       -- write this module to disk?
       )
      ]

    forM_ [OnDisk, InMemory] $ \sync ->
      -- This one fails unless CPP actually preprocessed the source
      go ("CPP_" ++ ppSync sync) ["A"]
        [( "A"
         , "{-# LANGUAGE CPP #-}"
         , ["#define y 1"]
         , [("x", "y")]
         , sync
         )
        ]

    -- These check if on-disk modules can import in-memory targets and
    -- vice-versa.
    forM_ (words "DD MM DM MD") $ \sync@[a_sync, b_sync] -> do
      dep <- return $ \y ->
         [( "A"
         , "{-# LANGUAGE CPP #-}"
         , ["import B"]
         , [("x", "y")]
         , readSync a_sync
         ),
         ( "B"
         , "{-# LANGUAGE CPP #-}"
         , []
         , [("y", y)]
         , readSync b_sync
         )
        ]
      go ("Dep_" ++ sync ++ "_AB")       ["A", "B"] (dep "()")

      -- This checks if error messages are correctly referring to the real
      -- source file and not the temp preprocessor input file.
      go ("Dep_Error_" ++ sync ++ "_AB") ["A", "B"] (dep "z")

      -- Try with only one target, this is expected to fail with a module
      -- not found error where module B is not OnDisk.
      go ("Dep_Error_" ++ sync ++ "_A")  ["A"]      (dep "z")

    return ()

data Sync
    = OnDisk   -- | Write generated module to disk
    | InMemory -- | Only fill in targetContents.

ppSync OnDisk   = "D"
ppSync InMemory = "M"

readSync 'D' = OnDisk
readSync 'M' = InMemory

type Mod =
  ( String -- module name
  , String -- pragmas
  , [String] -- imports
  , [(String, String)] -- bindings (LHS, RHS)
  , Sync -- Is the module on disk or just in memory?
  )

go :: String -> [String] -> [Mod] -> Ghc ()
go label targets mods = do
    liftIO $ createDirectoryIfMissing False "./outdir"
    setTargets []; _ <- load LoadAllTargets

    liftIO $ hPutStrLn stderr $ "== " ++ label
    t <- liftIO getCurrentTime
    setTargets =<< catMaybes <$> mapM (mkTarget t) mods
    ex <- MC.try $ load LoadAllTargets
    case ex of
      Left ex -> liftIO $ hPutStrLn stderr $ show (ex :: SourceError)
      Right _ -> return ()

    mapM_ (liftIO . cleanup) mods
    liftIO $ removeDirectoryRecursive "./outdir"

  where
    mkTarget t mod@(name,_,_,_,sync) = do
      src <- liftIO $ genMod mod
      unit <- hscActiveUnit <$> getSession
      return $ if not (name `elem` targets)
         then Nothing
         else Just $ Target
           { targetId = TargetFile (name++".hs") Nothing
           , targetAllowObjCode = False
           , targetUnitId = unit
           , targetContents =
               case sync of
                 OnDisk -> Nothing
                 InMemory ->
                   Just ( stringToStringBuffer src
                        , t
                        )
           }

genMod :: (String, String, [String], [(String, String)], Sync) -> IO String
genMod (mod, pragmas, internal, binders, sync) = do
    case sync of
      OnDisk   -> writeFile (mod++".hs") src
      InMemory -> return ()
    return src
  where
    exports = intercalate ", " $ map fst binders
    decls = map (\(b,v) -> b ++ " = " ++ v) binders
    src = unlines $
      [ pragmas
      , "module " ++ mod ++ " ("++ exports ++") where"
      ] ++ internal ++ decls

cleanup :: (String, String, [String], [(String, String)], Sync) -> IO ()
cleanup (mod,_,_,_,OnDisk) = removeFile (mod++".hs")
cleanup _ = return ()
