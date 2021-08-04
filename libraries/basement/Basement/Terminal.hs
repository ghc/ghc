{-# LANGUAGE CPP #-}
module Basement.Terminal
    ( initialize
    , getDimensions
    ) where

import Basement.Compat.Base
import Basement.Terminal.Size (getDimensions)
#ifdef mingw32_HOST_OS
import System.IO (hSetEncoding, utf8, hPutStrLn, stderr, stdin, stdout)
import System.Win32.Console (setConsoleCP, setConsoleOutputCP, getConsoleCP, getConsoleOutputCP)
#endif

initialize :: IO ()
initialize = do
#ifdef mingw32_HOST_OS
    query getConsoleOutputCP (\e -> setConsoleOutputCP e >> hSetEncoding stdout utf8 >> hSetEncoding stderr utf8) utf8Code
    query getConsoleCP (\e -> setConsoleCP e >> hSetEncoding stdin utf8) utf8Code
  where
    utf8Code = 65001
    query get set expected = do
        v <- get
        if v == expected then pure () else set expected
#else
    pure ()
#endif
