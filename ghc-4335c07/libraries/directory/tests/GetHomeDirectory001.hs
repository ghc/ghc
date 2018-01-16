{-# LANGUAGE CPP #-}
module GetHomeDirectory001 where
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do
  homeDir <- getHomeDirectory
  T(expect) () (homeDir /= "") -- sanity check
  _ <- getAppUserDataDirectory   "test"
  _ <- getXdgDirectory XdgCache  "test"
  _ <- getXdgDirectory XdgConfig "test"
  _ <- getXdgDirectory XdgData   "test"
  _ <- getUserDocumentsDirectory
  _ <- getTemporaryDirectory
  return ()
