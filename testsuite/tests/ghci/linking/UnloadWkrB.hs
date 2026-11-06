module UnloadWkrB where

-- Deliberately refers to a symbol that does not exist anywhere, so that
-- resolveObjs fails when this module's object code is linked into the
-- interpreter.  That is the only way to reach the unload_wkr call in
-- GHC.Linker.Loader.loadObjects.
foreign import ccall unsafe "unload_wkr_throw_missing_symbol"
  c_boom :: IO ()

boom :: IO ()
boom = c_boom
