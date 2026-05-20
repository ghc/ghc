module Main where

-- base
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

-- time
import Data.Time (getCurrentTime)

-- ghc
import qualified GHC as GHC
import qualified GHC.Core as GHC
import qualified GHC.Data.StringBuffer as GHC
import qualified GHC.Unit.Module.ModGuts as GHC
import qualified GHC.Unit.Types as GHC

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let inputSource = unlines
          [ "module NumLitDesugaring where"
          , "f :: Num a => a" -- !!! Succeeds if type signature is f :: Int
          , "f = 1"
          ]

    void $ compileToCore "NumLitDesugaring" inputSource

compileToCore :: String -> String -> IO [GHC.CoreBind]
compileToCore modName inputSource = do
    [libdir] <- getArgs
    GHC.runGhc (Just libdir) $ do
      (_ms, tcMod) <- typecheckSourceCode modName inputSource
      dsMod <- GHC.desugarModule tcMod
      return $ GHC.mg_binds $ GHC.dm_core_module dsMod

typecheckSourceCode
  :: GHC.GhcMonad m => String -> String -> m (GHC.ModSummary, GHC.TypecheckedModule)
typecheckSourceCode modName inputSource = do
    now <- liftIO getCurrentTime
    df1 <- GHC.getSessionDynFlags
    GHC.setSessionDynFlags $ df1 { GHC.backend = GHC.bytecodeBackend }
    let target = GHC.Target
               { GHC.targetId           = GHC.TargetFile (modName ++ ".hs") Nothing
               , GHC.targetUnitId       = GHC.homeUnitId_ df1
               , GHC.targetAllowObjCode = False
               , GHC.targetContents     = Just (GHC.stringToStringBuffer inputSource, now)
               }
    GHC.setTargets [target]
    void $ GHC.depanal [] False

    ms <- GHC.getModSummary
            (GHC.mkModule GHC.mainUnit (GHC.mkModuleName modName))
    tm <- GHC.parseModule ms >>= GHC.typecheckModule GHC.NoTcMPlugins
    return (ms, tm)
