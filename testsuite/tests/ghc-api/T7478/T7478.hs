{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Data.List ((\\))
import Control.Monad (void)
import System.Environment

import GHC
import GHC.Driver.Env
import qualified GHC.Settings.Config as GHC
import qualified GHC.Utils.Outputable as GHC
import qualified GHC.Driver.Ppr as GHC
import GHC.Driver.Monad (liftIO)
import GHC.Utils.Outputable (PprStyle, queryQual)
import GHC.Unit.State
import GHC.Types.Error

compileInGhc :: [FilePath]          -- ^ Targets
             -> (String -> IO ())   -- ^ handler for each MCOutput message
             -> Ghc ()
compileInGhc targets handlerOutput = do
    -- Set flags
    flags0 <- getSessionDynFlags
    let flags = flags0 {verbosity = 1 }
    setSessionDynFlags flags
    let collectSrcError handlerOutput _flags MCOutput _srcspan msg
          = handlerOutput $ GHC.showSDocForUser flags emptyUnitState alwaysQualify msg
        collectSrcError _ _ _ _ _
          = return ()
    pushLogHookM (const (collectSrcError handlerOutput))
    -- Set up targets.
    oldTargets <- getTargets
    let oldFiles = map fileFromTarget oldTargets
    unit <- hscActiveUnit <$> getSession
    mapM_ (\filename -> addSingle filename unit) (targets \\ oldFiles)
    mapM_ (removeTarget . targetIdFromFile) $ oldFiles \\ targets
    -- Load modules to typecheck
    void $ load LoadAllTargets
  where
    targetIdFromFile file = TargetFile file Nothing

    addSingle filename unitId =
      addTarget Target
        { targetId           = targetIdFromFile filename
        , targetAllowObjCode = True
        , targetUnitId       = unitId
        , targetContents     = Nothing
        }

    fileFromTarget Target{targetId} =
      case targetId of
        TargetFile file Nothing -> file
        _ -> error "fileFromTarget: not a known target"


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
