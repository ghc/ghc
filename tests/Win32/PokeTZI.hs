module Main where

import Control.Exception (assert)
import Foreign
import System.Win32.Time

main :: IO ()
main = do
    (_, tzi) <- getTimeZoneInformation
    alloca $ \buf -> do
        poke buf tzi
        tzi' <- peek buf
        assert (tzi == tzi') $ return ()
