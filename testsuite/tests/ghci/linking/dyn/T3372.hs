-- Note: This test exercises running concurrent GHCi sessions, but
-- although this test is expected to pass, running concurrent GHCi
-- sessions is currently broken in other ways; see #24345.
{-# LANGUAGE MagicHash #-}

module Main where

import Prelude hiding ( init )
import System.Environment

import Control.Monad ( join, forever )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan

import GHC ( Ghc )
import qualified GHC
import qualified GHC.Utils.Monad as GHC

import qualified GHC.Exts

main :: IO ()
main = do let test1 = "TestMain1.hs"
          let test2 = "TestMain2.hs"
          writeFile test1 "module Main where main = return () ; test1 = (1,2,3)"
          writeFile test2 "module Main where main = return () ; test2 = (3,2,1)"
          --
          ghc_1 <- newGhcServer
          ghc_2 <- newGhcServer
          line "1" $ runInServer ghc_1 $ load (test1, "Main")
          line "2" $ runInServer ghc_2 $ load (test2, "Main")
          line "3" $ runInServer ghc_1 $ eval "test1"
          line "4" $ runInServer ghc_2 $ eval "test2"
  where line n a = putStr (n ++ ": ") >> a

type ModuleName = String
type GhcServerHandle = Chan (Ghc ())

newGhcServer :: IO GhcServerHandle
newGhcServer = do (libdir:_) <- getArgs
                  pChan <- newChan
                  let be_a_server = forever $ join (GHC.liftIO $ readChan pChan)
                  forkIO $ ghc be_a_server libdir
                  return pChan
  where ghc action libdir = GHC.runGhc (Just libdir) (init >> action)
        init = do df <- GHC.getSessionDynFlags
                  GHC.setSessionDynFlags df{GHC.ghcMode    = GHC.CompManager,
                                            GHC.backend    = GHC.interpreterBackend,
                                            GHC.ghcLink    = GHC.LinkInMemory,
                                            GHC.verbosity  = 0}

runInServer :: GhcServerHandle -> Ghc a -> IO a
runInServer h action = do me <- newChan
                          writeChan h $ action >>= (GHC.liftIO . writeChan me)
                          readChan me

load :: (FilePath,ModuleName) -> Ghc ()
load (f,mn) = do target <- GHC.guessTarget f Nothing Nothing
                 GHC.setTargets [target]
                 res <- GHC.load GHC.LoadAllTargets
                 GHC.liftIO $ putStrLn ("Load " ++ showSuccessFlag res)
                 --
                 m <- GHC.findModule (GHC.mkModuleName mn) Nothing
                 GHC.setContext [GHC.IIModule $ GHC.moduleName $ m]
    where showSuccessFlag GHC.Succeeded = "succeeded"
          showSuccessFlag GHC.Failed    = "failed"

eval :: String -> Ghc ()
eval e = do show_e <- GHC.compileExpr $ "(show ("++ e ++")) :: String"
            GHC.liftIO $ putStrLn (GHC.Exts.unsafeCoerce# show_e)
