{-# LANGUAGE CPP #-}

module GHC.Utils.Touch (touch) where

import GHC.Prelude

#if defined(mingw32_HOST_OS)
import System.Win32.File
import System.Win32.Time
#else
import System.Posix.Files
import System.Posix.IO
#endif

-- | Set the mtime of the given file to the current time.
touch :: FilePath -> IO ()
touch file = do
#if defined(mingw32_HOST_OS)
    hdl <- createFile file gENERIC_WRITE fILE_SHARE_NONE Nothing oPEN_ALWAYS fILE_ATTRIBUTE_NORMAL Nothing
    t <- getSystemTimeAsFileTime
    setFileTime hdl Nothing Nothing (Just t)
    closeHandle hdl
#else
  let oflags = defaultFileFlags { noctty = True, creat = Just 0o666 }
  fd <- openFd file WriteOnly oflags
  touchFd fd
  closeFd fd
#endif
