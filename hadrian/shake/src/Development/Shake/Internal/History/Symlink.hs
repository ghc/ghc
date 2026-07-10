{-# LANGUAGE CPP #-}

module Development.Shake.Internal.History.Symlink(
    copyFileLink,
    createLinkMaybe
    ) where

import Control.Monad.Extra
import General.Extra
import System.Directory
import System.FilePath


#ifdef mingw32_HOST_OS
import Foreign.Ptr
import Foreign.C.String
#else
import System.Posix.Files(createLink)
#endif

createLinkMaybe :: FilePath -> FilePath -> IO (Maybe String)

#ifdef mingw32_HOST_OS

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "Windows.h CreateHardLinkW " c_CreateHardLinkW :: CWString -> CWString -> Ptr () -> IO Bool

createLinkMaybe from to = withCWString from $ \cfrom -> withCWString to $ \cto -> do
    res <- c_CreateHardLinkW cto cfrom nullPtr
    pure $ if res then Nothing else Just "CreateHardLink failed."

#else

createLinkMaybe from to = handleIO (pure . Just . show) $ createLink from to >> pure Nothing

#endif


copyFileLink :: Bool -> FilePath -> FilePath -> IO ()
copyFileLink useSymlink from to = do
    createDirectoryRecursive $ takeDirectory to
    removeFile_ to
    if not useSymlink then copyFile from to else do
        b <- createLinkMaybe from to
        whenJust b $ \_ ->
            copyFile from to
        -- making files read only stops them from inadvertently mutating the cache
        forM_ [from, to] $ \x -> do
            perm <- getPermissions x
            setPermissions x perm{writable=False}
