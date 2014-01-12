{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Data.List ((\\))
import Control.Monad (void)
import System.Environment

import GHC
import qualified Config as GHC
import qualified Outputable as GHC
import GhcMonad (liftIO)
import Outputable (PprStyle, qualName, qualModule)

compileInGhc :: [FilePath]          -- ^ Targets
             -> (String -> IO ())   -- ^ handler for each SevOutput message
             -> Ghc ()
compileInGhc targets handlerOutput = do
    -- Set flags
    flags0 <- getSessionDynFlags
    let flags = flags0 {verbosity = 1, log_action = collectSrcError handlerOutput}
    setSessionDynFlags flags
    -- Set up targets.
    oldTargets <- getTargets
    let oldFiles = map fileFromTarget oldTargets
    mapM_ addSingle (targets \\ oldFiles)
    mapM_ (removeTarget . targetIdFromFile) $ oldFiles \\ targets
    -- Load modules to typecheck
    void $ load LoadAllTargets
  where
    targetIdFromFile file = TargetFile file Nothing

    addSingle filename =
      addTarget Target
        { targetId           = targetIdFromFile filename
        , targetAllowObjCode = True
        , targetContents     = Nothing
        }

    fileFromTarget Target{targetId} =
      case targetId of
        TargetFile file Nothing -> file
        _ -> error "fileFromTarget: not a known target"

    collectSrcError handlerOutput flags SevOutput _srcspan style msg
      = handlerOutput $ GHC.showSDocForUser flags (qualName style,qualModule style) msg
    collectSrcError _ _ _ _ _ _
      = return ()

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do

  liftIO $ putStrLn "----- 0 ------"
  compileInGhc ["A.hs", "B.hs"] $ \msg -> print (0 :: Int, msg)

  liftIO $ putStrLn "----- 1 ------"
  compileInGhc ["A.hs", "B.hs"] $ \msg -> print (1 :: Int, msg)

  liftIO $ putStrLn "----- 2 ------"
  compileInGhc ["C.hs"] $ \msg -> print (2 :: Int, msg)
