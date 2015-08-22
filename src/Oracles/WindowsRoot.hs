{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.WindowsRoot (windowsRoot, windowsRootOracle) where

import Base

newtype WindowsRoot = WindowsRoot ()
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- Looks up cygwin/msys root on Windows
windowsRoot :: Action String
windowsRoot = askOracle $ WindowsRoot ()

-- Oracle for windowsRoot. This operation requires caching as looking up
-- the root is slow (at least the current implementation).
windowsRootOracle :: Rules ()
windowsRootOracle = do
    root <- newCache $ \() -> do
        Stdout out <- quietly $ cmd ["cygpath", "-m", "/"]
        let root = dropWhileEnd isSpace out
        putOracle $ "Detected root on Windows: " ++ root
        return root
    _ <- addOracle $ \WindowsRoot{} -> root ()
    return ()
