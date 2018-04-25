{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Control.Monad

import Foreign

import Types

import System.Environment
import System.Posix.DynamicLinker
import GHCi.ObjLink

rotateSO
  :: (FunPtr (IO (StablePtr a)) -> (IO (StablePtr a)))
  -> String
  -> (Maybe FilePath, FilePath)
  -> IO a
rotateSO dynamicCall symName (old, newDLL) = do
  -- initObjLinker is idempotent
  initObjLinker DontRetainCAFs

  loadObj newDLL
  resolved <- resolveObjs
  unless resolved $
    throwIO (ErrorCall $ "Unable to resolve objects for " ++ newDLL)
  c_sym <- lookupSymbol symName
  h <- case c_sym of
    Nothing -> throwIO (ErrorCall "Could not find symbol")
    Just p_sym ->
      bracket (dynamicCall $ castPtrToFunPtr p_sym) freeStablePtr deRefStablePtr
  purgeObj newDLL
  forM_ old unloadObj
  return h

foreign import ccall "dynamic"
  mkCallable :: FunPtr SOHandleExport -> SOHandleExport

main :: IO ()
main = do
  [file] <- getArgs
  SOHandles{..} <- rotateSO mkCallable "hs_soHandles" (Nothing, file)
  someFn 7
  putStrLn $ "someData = " ++ show someData
