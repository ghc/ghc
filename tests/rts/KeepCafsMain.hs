module Main (main) where

import Foreign
import GHCi.ObjLink
import System.Mem
import System.Exit

foreign import ccall "dynamic"
  callGetX :: FunPtr (IO Int) -> IO Int

main :: IO ()
main = do
  initObjLinker DontRetainCAFs
  let
    loadAndCall obj = do
      loadObj obj
      resolveObjs
      r <- lookupSymbol "getX"
      case r of
        Nothing -> die "cannot find getX"
        Just ptr -> callGetX (castPtrToFunPtr ptr) >>= print
      unloadObj obj
      performGC
  loadAndCall "KeepCafs1.o"
  loadAndCall "KeepCafs2.o"
